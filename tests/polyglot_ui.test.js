import test from "node:test";
import assert from "node:assert/strict";
import {
  compileToReact,
  compileToVue,
  compileToSvelte,
  compileToSSR,
  transpile
} from "../bridges/transpiler.js";

test("Universal Polyglot UI Transpiler - Card Component Cross-Compilation", () => {
  const cardSpec = {
    name: "Card",
    propsType: "CardProps",
    props: [
      { name: "title", type: "string" },
      { name: "description", type: "string" }
    ],
    root: {
      tag: "div",
      attrs: { class: "p-4 rounded-xl border border-line bg-surface" },
      children: [
        { tag: "h3", attrs: { class: "font-semibold text-lg text-ink" }, children: ["{props.title}"] },
        { tag: "p", attrs: { class: "text-sm text-ink-muted mt-2" }, children: ["{props.description}"] }
      ]
    }
  };

  // React 19 TSX
  const reactOut = compileToReact(cardSpec);
  assert.match(reactOut, /import React, \{ useState \} from "react";/);
  assert.match(reactOut, /export interface CardProps/);
  assert.match(reactOut, /className="p-4 rounded-xl border border-line bg-surface"/);
  assert.match(reactOut, /export const Card = \(props: CardProps\) => \{/);

  // Vue 3 SFC
  const vueOut = compileToVue(cardSpec);
  assert.match(vueOut, /<script setup lang="ts">/);
  assert.match(vueOut, /const props = withDefaults\(defineProps<CardProps>\(\),/);
  assert.match(vueOut, /<template>/);
  assert.match(vueOut, /class="p-4 rounded-xl border border-line bg-surface"/);
  assert.match(vueOut, /\{\{ props.title \}\}/);

  // Svelte 5 Runes
  const svelteOut = compileToSvelte(cardSpec);
  assert.match(svelteOut, /<script lang="ts">/);
  assert.match(svelteOut, /let \{ className = "", title, description \}:.*= \$props\(\);/);
  assert.match(svelteOut, /<div class="p-4 rounded-xl border border-line bg-surface">/);

  // SSR HTML
  const ssrOut = compileToSSR(cardSpec.root);
  assert.match(ssrOut, /^<div class="p-4 rounded-xl border border-line bg-surface"><h3/);
  assert.match(ssrOut, /<\/div>$/);
});

test("Universal Polyglot UI Transpiler - Reactive Counter Component", () => {
  const counterSpec = {
    name: "Counter",
    propsType: "CounterProps",
    props: [
      { name: "step", type: "number", default: 1 }
    ],
    state: [
      { name: "count", type: "number", initial: 0 }
    ],
    handlers: {
      increment: "setCount(count + 1);",
      decrement: "setCount(count - 1);"
    },
    root: {
      tag: "div",
      attrs: { class: "flex items-center gap-4" },
      children: [
        {
          tag: "button",
          attrs: { class: "btn-secondary", onclick: "decrement" },
          children: ["-"]
        },
        {
          tag: "span",
          attrs: { class: "text-xl font-bold" },
          children: ["{count}"]
        },
        {
          tag: "button",
          attrs: { class: "btn-primary", onclick: "increment" },
          children: ["+"]
        }
      ]
    }
  };

  // React 19
  const reactCode = transpile(counterSpec, "react");
  assert.match(reactCode, /const \[count, setCount\] = useState<number>\(0\);/);
  assert.match(reactCode, /const increment = \(\) => \{\s+setCount\(count \+ 1\);/);
  assert.match(reactCode, /onClick=\{increment\}/);

  // Vue 3
  const vueCode = transpile(counterSpec, "vue");
  assert.match(vueCode, /import \{ ref \} from "vue";/);
  assert.match(vueCode, /const count = ref<number>\(0\);/);
  assert.match(vueCode, /count\.value = count\.value \+ 1;/);
  assert.match(vueCode, /@click="increment"/);
  assert.match(vueCode, /\{\{ count \}\}/);

  // Svelte 5
  const svelteCode = transpile(counterSpec, "svelte");
  assert.match(svelteCode, /let count = \$state<number>\(0\);/);
  assert.match(svelteCode, /count = count \+ 1;/);
  assert.match(svelteCode, /onclick=\{increment\}/);

  // SSR HTML
  const ssrCode = transpile(counterSpec, "html");
  assert.doesNotMatch(ssrCode, /onclick/i); // Client event handlers stripped in SSR
  assert.match(ssrCode, /<div class="flex items-center gap-4">/);
});

test("Universal Polyglot UI Transpiler - Void Elements & Attributes", () => {
  const formSpec = {
    tag: "form",
    attrs: { class: "space-y-4" },
    children: [
      {
        tag: "input",
        attrs: { type: "text", placeholder: "Agent Query", name: "query" }
      },
      {
        tag: "img",
        attrs: { src: "/logo.svg", alt: "ASL Logo" }
      }
    ]
  };

  const reactForm = compileToReact(formSpec);
  assert.match(reactForm, /<input type="text" placeholder="Agent Query" name="query" \/>/);
  assert.match(reactForm, /<img src="\/logo.svg" alt="ASL Logo" \/>/);

  const ssrForm = compileToSSR(formSpec);
  assert.match(ssrForm, /<input type="text" placeholder="Agent Query" name="query" \/>/);
  assert.match(ssrForm, /<img src="\/logo.svg" alt="ASL Logo" \/>/);
});
