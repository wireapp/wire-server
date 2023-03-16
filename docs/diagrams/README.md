# Diagrams with Kroki / Mermaid

This is a "diagrams playground" folder and you don't have to use this. Instead, you can create diagrams in a .rst file inside ..//src with this example kroki/mermaid syntax and use the normal development setup described in ../README.md to get live previews:

```rst
.. kroki::
   :type: mermaid
   :caption: Diagram

   %% The message sending flow as implemented for M1 (Oct 2021) federation
   sequenceDiagram
       actor A as Alice
       participant B as Backend

       Note over A,B: send message
       %% this is a code-comment not visible
       A->>+B: Hello!
       B->>-A: Greetings!
```

To create diagrams in a markdown file, you can use the same syntax as above but wrap it in triple backticks (used for code blocks) with `{eval-rst}` as the language identifier, e.g.:

````markdown
```{eval-rst}
.. kroki::
   :type: mermaid
   :caption: Diagram

   %% The message sending flow as implemented for M1 (Oct 2021) federation
   sequenceDiagram
       actor A as Alice
       participant B as Backend

       Note over A,B: send message
       %% this is a code-comment not visible
       A->>+B: Hello!
       B->>-A: Greetings!!!!
```
````

For more information, see syntax on https://mermaid-js.github.io/mermaid/#/sequenceDiagram

# Playground mermaid setup and further links

[OPTIONAL] For an alternative setup and debugging; or working with mermaid directly without kroki, you can use this ./diagrams folder and commands.

* try them out on https://mermaid.live/
* locally compile them with `./mmdc` to `svg`: [mmdc / mermaid-cli](https://github.com/mermaid-js/mermaid-cli)
    * `npm install @mermaid-js/mermaid-cli`
* see mermaid [integrations](https://mermaid-js.github.io/mermaid/#/./integrations)
* type 'make watch' if you have `mmdc`, a pdf viewer (e.g. `okular` or similar) and `entr` locally available for one option of a "save + autoreload" workflow.
    * another option is to write sphinx/rst files and use `make dev-run` from the top-level folder. That might be easier.
