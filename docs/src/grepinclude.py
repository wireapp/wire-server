import re
from docutils import nodes
from sphinx.util.docutils import SphinxDirective
from sphinx.directives.code import container_wrapper
from sphinx.util import logging

logger = logging.getLogger(__name__)


class GrepInclude(SphinxDirective):
    """
    A bit like 'literalinclude', but using a keyword to highlight some code so
    as to be a little more resilient to code moving lines with time.
    Usage:
    ```{grepinclude} ../charts/coturn/values.yaml ciphers:
    ---
    lines-before: 3
    lines-after: 0
    language: yaml
    ---
    ```

    or alternatively

    ```{eval-rst}
    .. grepinclude:: ../charts/coturn/values.yaml ciphers:
       :lines-before: 3
       :lines-after: 0
       :language: yaml
    ```
    """
    required_arguments = 2
    optional_arguments = 0
    final_argument_whitespace = True
    option_spec = {
        'lines-before': int,
        'lines-after': int,
        'language': str,
    }

    def run(self):
        file_path, keyword = self.arguments
        lines_before = self.options.get('lines-before', 0)
        lines_after = self.options.get('lines-after', 0)
        language = self.options.get('language', 'none')

        try:
            with open(file_path, 'r') as f:
                content = f.readlines()
        except FileNotFoundError:
            source, line = self.state_machine.get_source_and_line()
            err = f"""
            Unable to open file {file_path}
            GrepInclude called on line {line} in {source}
            """
            logger.error(err)
            return [nodes.error(
                None, nodes.paragraph(
                    text=f"Unable to open file: {file_path}")
            )]

        filtered_lines = []
        for i, line in enumerate(content):
            if re.search(keyword, line):
                start = max(0, i - lines_before)
                end = min(len(content), i + 1 + lines_after)
                filtered_lines.extend(content[start:end])

        if len(filtered_lines) == 0:
            source, line = self.state_machine.get_source_and_line()
            err = f"""Unable to find substring '{keyword}'
            in file {file_path}".
            GrepInclude called on line {line}
            in {source}
            """
            logger.error(err)
            return [nodes.error(
                None, nodes.paragraph(
                    text=err)
            )]

        text = ''.join(filtered_lines)
        code_node = nodes.literal_block(text, text)
        code_node['language'] = language
        caption = file_path
        code_node = container_wrapper(self, code_node, caption)
        self.add_name(code_node)

        return [code_node]


def setup(app):
    app.add_directive('grepinclude', GrepInclude)
