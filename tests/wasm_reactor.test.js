import test from "node:test";
import assert from "node:assert/strict";
import {
  WasmMemoryVirtualizer,
  WasmReactor,
  WebGPUComputePipeline
} from "../bridges/wasm_reactor.js";

test("WasmMemoryVirtualizer - Aligned Allocation & Buffer Growth", () => {
  const virt = new WasmMemoryVirtualizer(1, 4); // 1 initial page (64KB)
  assert.equal(virt.memory.buffer.byteLength, 65536);

  // Allocate 128 bytes
  const ptr1 = virt.allocate(128);
  assert.equal(ptr1 % 8, 0); // 8-byte aligned

  // Write and read bytes
  const testData = new Uint8Array([1, 2, 3, 4, 5, 42, 99]);
  virt.writeBytes(ptr1, testData);
  const readBack = virt.readBytes(ptr1, testData.length);
  assert.deepEqual(readBack, testData);

  // Allocate past 64KB to trigger memory grow
  const ptrHuge = virt.allocate(70000);
  assert.ok(virt.memory.buffer.byteLength > 65536);
  assert.equal(ptrHuge % 8, 0);

  virt.reset();
  assert.equal(virt.offset, 64);
});

test("WasmReactor - Event Dispatch, Micro-batching & Subscriptions", async () => {
  const reactor = new WasmReactor({ batchWindowMs: 2 });
  await reactor.loadWasm(null); // Load mock instance

  let receivedBatch = null;
  const unsubscribe = reactor.subscribe((event) => {
    if (event.type === "batch") {
      receivedBatch = event;
    }
  });

  // Dispatch multiple events
  reactor.dispatch({ type: "click", target: "@e1", payload: { count: 1 } });
  reactor.dispatch({ type: "input", target: "@e2", payload: { value: "Hello" } });

  // Flush immediately
  const mutations = reactor.flush();
  assert.equal(mutations.length, 2);
  assert.equal(mutations[0].sourceEvent, "click");
  assert.equal(mutations[0].target, "@e1");
  assert.equal(mutations[1].sourceEvent, "input");
  assert.equal(mutations[1].target, "@e2");

  assert.ok(receivedBatch !== null);
  assert.equal(receivedBatch.mutations.length, 2);
  assert.ok(reactor.telemetry.eventsProcessed >= 2);
  assert.ok(reactor.telemetry.batchesDispatched >= 1);

  unsubscribe();
  reactor.reset();
});

test("WebGPUComputePipeline - Initialization & CPU/SIMD Fallback", async () => {
  const gpu = new WebGPUComputePipeline();
  const initialized = await gpu.init();
  // In Node.js environment, navigator.gpu is not available so it gracefully falls back
  assert.equal(typeof initialized, "boolean");

  const pipeline = gpu.createComputePipeline(`
    @group(0) @binding(0) var<storage, read_write> data: array<f32>;
    @compute @workgroup_size(64)
    fn main(@builtin(global_invocation_id) global_id: vec3<u32>) {
      data[global_id.x] = data[global_id.x] * 2.0;
    }
  `);

  assert.ok(typeof pipeline.dispatch === "function");
  const input = [1.0, 2.0, 3.0, 4.0];
  const output = await pipeline.dispatch(input);
  assert.deepEqual(Array.from(output), [2.0, 4.0, 6.0, 8.0]);
});

test("WasmReactor - Sub-millisecond Dispatch & Microtask Latency", () => {
  const reactor = new WasmReactor();
  const start = performance.now();
  for (let i = 0; i < 100; i++) {
    reactor.dispatch({ type: "hover", target: `@e${i}` });
  }
  const mutations = reactor.flush();
  const duration = performance.now() - start;

  assert.equal(mutations.length, 100);
  assert.ok(duration < 10.0, `Batch latency was ${duration}ms, expected < 10ms for 100 events`);
  reactor.reset();
});
