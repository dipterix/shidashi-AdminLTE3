import hljs from 'highlight.js/lib/core';
import matlab from 'highlight.js/lib/languages/matlab';
import python from 'highlight.js/lib/languages/python';
import yaml from 'highlight.js/lib/languages/yaml';
import r from 'highlight.js/lib/languages/r';
import markdown from 'highlight.js/lib/languages/markdown';

// import 'highlight.js/styles/github.css';


function registerHighlightJS () {
  hljs.registerLanguage('python', python);
  hljs.registerLanguage('py', python);
  hljs.registerLanguage('matlab', matlab);
  hljs.registerLanguage('r', r);
  hljs.registerLanguage('yaml', yaml);
  hljs.registerLanguage('markdown', markdown);
  hljs.registerLanguage('md', markdown);

  hljs.initHighlightingOnLoad();
  if (document.readyState && document.readyState === "complete") {
    window.setTimeout(function() { hljs.initHighlighting(); }, 0);
  }
}


export { registerHighlightJS };
