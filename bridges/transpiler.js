/**
 * Universal Polyglot UI Transpiler Bridge
 * Translates ASL declarative UI trees and component specs into:
 * 1. React 19 TSX (with Hooks and modern TS typings)
 * 2. Vue 3 Single-File Component (<script setup lang="ts"> with ref, defineProps)
 * 3. Svelte 5 component (Runes: $state, $props, $derived)
 * 4. SSR HTML (Zero-overhead static markup)
 */

export const VOID_TAGS = new Set([
  "input", "img", "br", "hr", "meta", "link", "area", "base", "col", "embed", "source", "track", "wbr"
]);

export function parseAslSExpr(text) {
  const trimmed = text.trim();
  if (!trimmed.startsWith("(") && !trimmed.startsWith("<")) {
    return { type: "text", text: trimmed };
  }

  let targetText = trimmed;
  if (trimmed.startsWith("(module")) {
    const match = trimmed.match(/\((div|section|header|footer|main|nav|card|span|p|h1|h2|h3|button|form)\b[\s\S]*\)/);
    if (match) {
      targetText = match[0];
    }
  }

  const tokens = [];
  let i = 0;
  while (i < targetText.length) {
    const ch = targetText[i];
    if (/\s/.test(ch)) {
      i++;
    } else if (ch === "(" || ch === ")") {
      tokens.push(ch);
      i++;
    } else if (ch === '"') {
      let str = "";
      i++;
      while (i < targetText.length && targetText[i] !== '"') {
        if (targetText[i] === '\\' && i + 1 < targetText.length) {
          str += targetText[i + 1];
          i += 2;
        } else {
          str += targetText[i];
          i++;
        }
      }
      i++;
      tokens.push({ type: "str", val: str });
    } else {
      let sym = "";
      while (i < targetText.length && !/[\s()]/.test(targetText[i])) {
        sym += targetText[i];
        i++;
      }
      tokens.push({ type: "sym", val: sym });
    }
  }

  let pos = 0;
  function parseExpr() {
    if (pos >= tokens.length) return null;
    const tok = tokens[pos++];
    if (tok === "(") {
      if (pos >= tokens.length) return null;
      const head = tokens[pos++];
      const tag = (typeof head === "object" && head.type === "sym") ? head.val : "div";
      const attrs = {};
      const children = [];

      while (pos < tokens.length && tokens[pos] !== ")") {
        const next = tokens[pos];
        if (next === "(") {
          if (pos + 1 < tokens.length && tokens[pos + 1]?.val?.startsWith(":")) {
            pos++;
            const attrKey = tokens[pos++].val.replace(/^:/, "");
            let attrVal = "";
            if (pos < tokens.length && tokens[pos] !== ")") {
              const v = tokens[pos++];
              attrVal = typeof v === "object" ? v.val : String(v);
            }
            if (pos < tokens.length && tokens[pos] === ")") pos++;
            attrs[attrKey] = attrVal;
          } else {
            const child = parseExpr();
            if (child) children.push(child);
          }
        } else if (typeof next === "object") {
          pos++;
          if (next.type === "str") {
            children.push({ type: "text", text: next.val });
          } else if (next.val.startsWith(":")) {
            const k = next.val.replace(/^:/, "");
            let v = "";
            if (pos < tokens.length && tokens[pos]?.type === "str") {
              v = tokens[pos++].val;
            }
            attrs[k] = v;
          } else {
            children.push({ type: "text", text: next.val });
          }
        } else {
          pos++;
        }
      }
      if (pos < tokens.length && tokens[pos] === ")") pos++;
      return { type: "element", tag, attrs, children };
    } else if (typeof tok === "object") {
      return { type: "text", text: tok.val };
    }
    return null;
  }

  const parsed = parseExpr();
  return parsed || { type: "text", text: trimmed };
}

/**
 * Parses an S-expression or JSON representation into a normalized VNode tree.
 */
export function normalizeVNode(node) {
  if (node === null || node === undefined) {
    return { type: "text", text: "" };
  }
  if (typeof node === "string") {
    if (node.trim().startsWith("(")) {
      return parseAslSExpr(node);
    }
    return { type: "text", text: node };
  }
  if (typeof node === "number" || typeof node === "boolean") {
    return { type: "text", text: String(node) };
  }
  if (node.type === "text" || node.text !== undefined) {
    return { type: "text", text: String(node.text ?? node.content ?? "") };
  }

  const tag = node.tag || node.name || "div";
  const attrs = { ...(node.attrs || node.props || {}) };
  const rawChildren = node.children || [];
  const children = Array.isArray(rawChildren) ? rawChildren.map(normalizeVNode) : [normalizeVNode(rawChildren)];

  return { type: "element", tag, attrs, children };
}

/**
 * Format indentation
 */
function indent(spaces = 0) {
  return " ".repeat(spaces);
}

/**
 * React 19 TSX Compiler
 */
export function compileToReact(spec, options = {}) {
  const componentName = spec.name || "Component";
  const propsTypeName = spec.propsType || `${componentName}Props`;
  const props = spec.props || [];
  const state = spec.state || [];
  const handlers = spec.handlers || {};
  const root = normalizeVNode(spec.root || spec);

  let out = `// React 19 Component generated by GenSEAM ASL Polyglot UI\n`;
  out += `import React, { useState } from "react";\n\n`;

  // Props interface
  out += `export interface ${propsTypeName} {\n`;
  out += `  className?: string;\n`;
  out += `  children?: React.ReactNode;\n`;
  for (const p of props) {
    out += `  ${p.name}?: ${p.type || "any"};\n`;
  }
  out += `}\n\n`;

  out += `export const ${componentName} = (props: ${propsTypeName}) => {\n`;

  // State hooks
  for (const s of state) {
    const capitalized = s.name.charAt(0).toUpperCase() + s.name.slice(1);
    const initial = typeof s.initial === "string" ? `"${s.initial}"` : JSON.stringify(s.initial ?? 0);
    out += `  const [${s.name}, set${capitalized}] = useState<${s.type || "any"}>(${initial});\n`;
  }
  if (state.length > 0) out += "\n";

  // Event handlers
  for (const [hName, hBody] of Object.entries(handlers)) {
    out += `  const ${hName} = () => {\n`;
    out += `    ${hBody}\n`;
    out += `  };\n`;
  }
  if (Object.keys(handlers).length > 0) out += "\n";

  out += `  return (\n`;
  out += renderReactJSX(root, 4);
  out += `\n  );\n`;
  out += `};\n\n`;
  out += `export default ${componentName};\n`;

  return out;
}

function renderReactJSX(node, level = 0) {
  const pad = indent(level);
  if (node.type === "text") {
    if (node.text.startsWith("{") && node.text.endsWith("}")) {
      return `${pad}${node.text}`;
    }
    return `${pad}${node.text}`;
  }

  const tag = node.tag;
  const attrParts = [];

  for (const [k, v] of Object.entries(node.attrs)) {
    let jsxKey = k;
    if (k === "class") jsxKey = "className";
    else if (k === "for") jsxKey = "htmlFor";
    else if (k === "tabindex") jsxKey = "tabIndex";
    else if (k.startsWith("on") && k.length > 2) {
      // Convert onclick -> onClick, onchange -> onChange
      jsxKey = "on" + k.charAt(2).toUpperCase() + k.slice(3);
    }

    if (String(v).startsWith("{") && String(v).endsWith("}")) {
      attrParts.push(`${jsxKey}=${v}`);
    } else if (k.startsWith("on") && typeof v === "string" && !v.includes("\"")) {
      attrParts.push(`${jsxKey}={${v}}`);
    } else {
      attrParts.push(`${jsxKey}="${v}"`);
    }
  }

  const attrStr = attrParts.length > 0 ? " " + attrParts.join(" ") : "";

  if (node.children.length === 0) {
    return VOID_TAGS.has(tag) ? `${pad}<${tag}${attrStr} />` : `${pad}<${tag}${attrStr}></${tag}>`;
  }

  if (node.children.length === 1 && node.children[0].type === "text") {
    return `${pad}<${tag}${attrStr}>${node.children[0].text}</${tag}>`;
  }

  const chLines = node.children.map(ch => renderReactJSX(ch, level + 2)).join("\n");
  return `${pad}<${tag}${attrStr}>\n${chLines}\n${pad}</${tag}>`;
}

/**
 * Vue 3 Single-File-Component Compiler
 */
export function compileToVue(spec, options = {}) {
  const componentName = spec.name || "Component";
  const propsTypeName = spec.propsType || `${componentName}Props`;
  const props = spec.props || [];
  const state = spec.state || [];
  const handlers = spec.handlers || {};
  const root = normalizeVNode(spec.root || spec);

  let out = `<script setup lang="ts">\n`;
  out += `// Vue 3 Component generated by GenSEAM ASL Polyglot UI\n`;
  if (state.length > 0) {
    out += `import { ref } from "vue";\n\n`;
  }

  out += `export interface ${propsTypeName} {\n`;
  out += `  className?: string;\n`;
  for (const p of props) {
    out += `  ${p.name}?: ${p.type || "any"};\n`;
  }
  out += `}\n\n`;

  out += `const props = withDefaults(defineProps<${propsTypeName}>(), {\n`;
  out += `  className: "",\n`;
  out += `});\n\n`;

  // Reactive state
  for (const s of state) {
    const initial = typeof s.initial === "string" ? `"${s.initial}"` : JSON.stringify(s.initial ?? 0);
    out += `const ${s.name} = ref<${s.type || "any"}>(${initial});\n`;
  }
  if (state.length > 0) out += "\n";

  // Handlers
  for (const [hName, hBody] of Object.entries(handlers)) {
    // Convert setter references like setCount(count + 1) to count.value++ if possible
    let vueBody = hBody;
    for (const s of state) {
      const cap = s.name.charAt(0).toUpperCase() + s.name.slice(1);
      const regex = new RegExp(`set${cap}\\(([^)]+)\\)`, "g");
      vueBody = vueBody.replace(regex, `${s.name}.value = $1`);
      vueBody = vueBody.replace(new RegExp(`\\b${s.name}\\b(?![.])`, "g"), `${s.name}.value`);
    }
    out += `function ${hName}() {\n  ${vueBody}\n}\n`;
  }

  out += `</script>\n\n`;
  out += `<template>\n`;
  out += renderVueTemplate(root, 2);
  out += `\n</template>\n`;

  return out;
}

function renderVueTemplate(node, level = 0) {
  const pad = indent(level);
  if (node.type === "text") {
    let t = node.text;
    if (t.startsWith("{") && t.endsWith("}")) {
      t = `{{ ${t.slice(1, -1).trim()} }}`;
    }
    return `${pad}${t}`;
  }

  const tag = node.tag;
  const attrParts = [];

  for (const [k, v] of Object.entries(node.attrs)) {
    if (k === "onclick") {
      attrParts.push(`@click="${v}"`);
    } else if (k === "oninput") {
      attrParts.push(`@input="${v}"`);
    } else if (k === "onchange") {
      attrParts.push(`@change="${v}"`);
    } else if (k === "class") {
      attrParts.push(`class="${v}"`);
    } else if (String(v).startsWith("{") && String(v).endsWith("}")) {
      attrParts.push(`:${k}="${v.slice(1, -1).trim()}"`);
    } else {
      attrParts.push(`${k}="${v}"`);
    }
  }

  const attrStr = attrParts.length > 0 ? " " + attrParts.join(" ") : "";

  if (node.children.length === 0) {
    return VOID_TAGS.has(tag) ? `${pad}<${tag}${attrStr} />` : `${pad}<${tag}${attrStr}></${tag}>`;
  }

  if (node.children.length === 1 && node.children[0].type === "text") {
    let t = node.children[0].text;
    if (t.startsWith("{") && t.endsWith("}")) {
      t = `{{ ${t.slice(1, -1).trim()} }}`;
    }
    return `${pad}<${tag}${attrStr}>${t}</${tag}>`;
  }

  const chLines = node.children.map(ch => renderVueTemplate(ch, level + 2)).join("\n");
  return `${pad}<${tag}${attrStr}>\n${chLines}\n${pad}</${tag}>`;
}

/**
 * Svelte 5 Runes Compiler
 */
export function compileToSvelte(spec, options = {}) {
  const componentName = spec.name || "Component";
  const props = spec.props || [];
  const state = spec.state || [];
  const handlers = spec.handlers || {};
  const root = normalizeVNode(spec.root || spec);

  let out = `<script lang="ts">\n`;
  out += `// Svelte 5 Runes Component generated by GenSEAM ASL Polyglot UI\n`;

  // Svelte 5 $props() rune
  const propDecls = [`className = ""`];
  const propTypes = [`className?: string`];
  for (const p of props) {
    propDecls.push(p.default !== undefined ? `${p.name} = ${JSON.stringify(p.default)}` : p.name);
    propTypes.push(`${p.name}?: ${p.type || "any"}`);
  }
  out += `  let { ${propDecls.join(", ")} }: { ${propTypes.join("; ")} } = $props();\n\n`;

  // Svelte 5 $state() runes
  for (const s of state) {
    const initial = typeof s.initial === "string" ? `"${s.initial}"` : JSON.stringify(s.initial ?? 0);
    out += `  let ${s.name} = $state<${s.type || "number"}>(${initial});\n`;
  }
  if (state.length > 0) out += "\n";

  // Event handlers
  for (const [hName, hBody] of Object.entries(handlers)) {
    let svelteBody = hBody;
    for (const s of state) {
      const cap = s.name.charAt(0).toUpperCase() + s.name.slice(1);
      const regex = new RegExp(`set${cap}\\(([^)]+)\\)`, "g");
      svelteBody = svelteBody.replace(regex, `${s.name} = $1`);
    }
    out += `  function ${hName}() {\n    ${svelteBody}\n  }\n`;
  }

  out += `</script>\n\n`;
  out += renderSvelteTemplate(root, 0);
  out += `\n`;

  return out;
}

function renderSvelteTemplate(node, level = 0) {
  const pad = indent(level);
  if (node.type === "text") {
    return `${pad}${node.text}`;
  }

  const tag = node.tag;
  const attrParts = [];

  for (const [k, v] of Object.entries(node.attrs)) {
    if (k === "onclick" || k === "onClick") {
      attrParts.push(`onclick={${v}}`);
    } else if (k === "oninput" || k === "onInput") {
      attrParts.push(`oninput={${v}}`);
    } else if (k === "class") {
      attrParts.push(`class="${v}"`);
    } else if (String(v).startsWith("{") && String(v).endsWith("}")) {
      attrParts.push(`${k}=${v}`);
    } else {
      attrParts.push(`${k}="${v}"`);
    }
  }

  const attrStr = attrParts.length > 0 ? " " + attrParts.join(" ") : "";

  if (node.children.length === 0) {
    return VOID_TAGS.has(tag) ? `${pad}<${tag}${attrStr} />` : `${pad}<${tag}${attrStr}></${tag}>`;
  }

  if (node.children.length === 1 && node.children[0].type === "text") {
    return `${pad}<${tag}${attrStr}>${node.children[0].text}</${tag}>`;
  }

  const chLines = node.children.map(ch => renderSvelteTemplate(ch, level + 2)).join("\n");
  return `${pad}<${tag}${attrStr}>\n${chLines}\n${pad}</${tag}>`;
}

/**
 * Zero-overhead SSR HTML Compiler
 */
export function compileToSSR(node) {
  const root = normalizeVNode(node);
  return renderSSR(root);
}

function renderSSR(node) {
  if (node.type === "text") {
    return escapeHtml(node.text);
  }

  const tag = node.tag;
  const attrParts = [];
  for (const [k, v] of Object.entries(node.attrs)) {
    if (k.startsWith("on")) continue; // Strip client handlers in SSR
    attrParts.push(`${k}="${escapeHtml(String(v))}"`);
  }
  const attrStr = attrParts.length > 0 ? " " + attrParts.join(" ") : "";

  if (VOID_TAGS.has(tag)) {
    return `<${tag}${attrStr} />`;
  }

  const childrenHtml = node.children.map(renderSSR).join("");
  return `<${tag}${attrStr}>${childrenHtml}</${tag}>`;
}

function escapeHtml(str) {
  return str
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;");
}

/**
 * Universal Transpile Dispatcher
 */
export function transpile(spec, target = "react", options = {}) {
  const normTarget = target.toLowerCase().trim();
  switch (normTarget) {
    case "react":
    case "tsx":
    case "jsx":
      return compileToReact(spec, options);
    case "vue":
    case "vue3":
    case "sfc":
      return compileToVue(spec, options);
    case "svelte":
    case "svelte5":
      return compileToSvelte(spec, options);
    case "html":
    case "ssr":
      return compileToSSR(spec.root || spec);
    default:
      throw new Error(`Unsupported UI transpile target: ${target}`);
  }
}
