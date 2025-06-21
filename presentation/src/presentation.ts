import "./app.css";

import Reveal from "reveal.js";
import Highlight from "reveal.js/plugin/highlight/highlight";
import Markdown from "reveal.js/plugin/markdown/markdown";
import Notes from "reveal.js/plugin/notes/notes";

// @ts-ignore
import RevealMermaid from "reveal.js-mermaid-plugin/plugin/mermaid/mermaid.js";

import "reveal.js/dist/reveal.css";
import "reveal.js/dist/theme/white.css";
import "./atom-one-light.min.css";

const deck = new Reveal({
  plugins: [Markdown, Highlight, Notes, RevealMermaid],
  slideNumber: true,
  hash: true,
});

deck.initialize();
