#!/usr/bin/env python3
"""Fix markdown ticket files for Jira upload.

1. Join paragraph lines (no hard breaks inside paragraphs)
2. Join list-item continuation lines
3. Ensure metadata has assignedTo + links in correct order
4. Replace filename refs (plan-otp-NN-xxx.md) with ticket title refs (TOTP NN)
"""

import glob
import os
import re
import sys


def join_paragraphs_and_continuations(text):
    """Join paragraph lines and list-item continuation lines into single lines."""
    lines = text.split('\n')
    result = []
    i = 0
    in_code_block = False

    while i < len(lines):
        line = lines[i]

        # Code fence
        if line.startswith('```'):
            result.append(line)
            in_code_block = not in_code_block
            i += 1
            continue

        if in_code_block:
            result.append(line)
            i += 1
            continue

        # Empty line
        if not line.strip():
            result.append('')
            i += 1
            continue

        # Heading
        if re.match(r'^#{1,6}\s', line):
            result.append(line)
            i += 1
            continue

        # Blockquote
        if line.startswith('>'):
            result.append(line)
            i += 1
            continue

        # Horizontal rule (must be at least 3 chars)
        if re.match(r'^[-_*]{3,}\s*$', line.strip()):
            result.append(line)
            i += 1
            continue

        # Table row
        if re.match(r'^\|', line.strip()):
            result.append(line)
            i += 1
            continue

        # List item (unordered or ordered) — collect continuations
        m = re.match(r'^(\s*)((?:[-*+]|\d+\.))\s', line)
        if m:
            indent = m.group(1)
            item_lines = [line.strip()]
            i += 1
            while i < len(lines):
                nl = lines[i]
                if not nl.strip():
                    break
                if nl.startswith('```'):
                    break
                if re.match(r'^#{1,6}\s', nl):
                    break
                if nl.startswith('>'):
                    break
                # New list item at any level — stop
                if re.match(r'^\s*[-*+]\s', nl) or re.match(r'^\s*\d+\.\s', nl):
                    break
                # Continuation line
                item_lines.append(nl.strip())
                i += 1
            joined = ' '.join(item_lines)
            result.append(indent + joined)
            continue

        # Regular paragraph text — collect consecutive non-special lines
        para_lines = [line]
        i += 1
        while i < len(lines):
            nl = lines[i]
            if (nl.strip()
                    and not nl.startswith('```')
                    and not re.match(r'^#{1,6}\s', nl)
                    and not re.match(r'^\s*[-*+]\s', nl)
                    and not re.match(r'^\s*\d+\.\s', nl)
                    and not nl.startswith('>')
                    and not re.match(r'^[-_*]{3,}\s*$', nl.strip())
                    and not re.match(r'^\|', nl.strip())):
                para_lines.append(nl)
                i += 1
            else:
                break
        result.append(' '.join(para_lines))

    return '\n'.join(result)


def fix_metadata(text):
    """Ensure metadata block has assignedTo and links in the right order.

    Desired order: assignedTo, blockedBy (if present), links.
    The links URL is always the same spec page.
    """
    lines = text.split('\n')

    # Find title line (first # heading containing TOTP NN)
    title_idx = None
    for idx, l in enumerate(lines):
        if re.match(r'^#\s+TOTP\s+\d+', l):
            title_idx = idx
            break
    if title_idx is None:
        return text

    # Check if there's already a metadata block after the title
    meta_start = None
    search_start = title_idx + 1
    for idx in range(search_start, min(search_start + 5, len(lines))):
        if re.match(r'^```', lines[idx].strip()):
            meta_start = idx
            break

    common_links = [
        'https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan',
    ]

    if meta_start is not None:
        # Find end of metadata block
        meta_end = None
        for idx in range(meta_start + 1, len(lines)):
            if lines[idx].strip() == '```':
                meta_end = idx
                break
        if meta_end is None:
            return text

        # Parse existing metadata
        raw_meta = lines[meta_start + 1:meta_end]
        assigned_to = None
        blocked_by = []
        links = []
        current_key = None
        for l in raw_meta:
            if l.startswith('assignedTo:'):
                assigned_to = l[11:].strip()
                current_key = None
            elif l.startswith('blockedBy:'):
                current_key = 'blockedBy'
            elif l.startswith('links:'):
                current_key = 'links'
            elif l.startswith('context:'):
                current_key = 'context'
            elif l.strip().startswith('- ') and current_key == 'blockedBy':
                blocked_by.append(l.strip()[2:])
            elif l.strip().startswith('- ') and current_key == 'links':
                links.append(l.strip()[2:])

        # Build new metadata
        new_meta = []
        new_meta.append(f'assignedTo: {assigned_to or "Matthias Fischmann"}')
        if blocked_by:
            new_meta.append('blockedBy:')
            for b in blocked_by:
                new_meta.append(f'- {b}')
        new_meta.append('links:')
        for link in common_links:
            new_meta.append(f'- {link}')
        for link in links:
            if link not in common_links:
                new_meta.append(f'- {link}')

        lines[meta_start] = '```'
        lines[meta_end] = '```'
        lines[meta_start + 1:meta_end] = new_meta
        return '\n'.join(lines)
    else:
        # No metadata block — insert one after the title
        blocked_by = []

        # Extract ticket number from title
        tm = re.match(r'^#\s+TOTP\s+(\d+)', lines[title_idx])
        num = int(tm.group(1)) if tm else 0

        # Determine blockedBy from the context (tickets usually block later tickets)
        # We'll derive this from knowledge: tickets are roughly sequential,
        # but let's just create the block without blockedBy unless it's obvious
        # from the file content.

        new_meta = ['```']
        new_meta.append(f'assignedTo: Matthias Fischmann')
        new_meta.append('links:')
        for link in common_links:
            new_meta.append(f'- {link}')
        new_meta.append('```')

        # Insert after title line (skip blank lines after title)
        insert_at = title_idx + 1
        while insert_at < len(lines) and not lines[insert_at].strip():
            insert_at += 1

        # Insert blank line before and after metadata
        prefix = lines[:title_idx + 1]
        suffix = lines[insert_at:]
        return '\n'.join(prefix + [''] + new_meta + [''] + suffix)


def fix_references(text):
    """Replace plan-otp-NN-xxx.md references with TOTP NN format.
    
    Keep plan-otp.md (the main spec doc) unchanged.
    Convert both:
      plan-otp-NN-xxx.md -> TOTP NN
      `plan-otp-NN`      -> `TOTP NN`  (backtick-wrapped)
      plan-otp-NN         -> TOTP NN   (bare)
    """
    def repl_filename(m):
        num = m.group(1)
        return f'TOTP {num}'
    def repl_bare(m):
        num = m.group(1)
        return f'TOTP {num}'
    # First handle filename references (with .md and topic)
    text = re.sub(r'plan-otp-(\d+)-[a-z0-9-]+\.md', repl_filename, text)
    # Then handle bare references (plan-otp-NN where NN is 2 digits, not followed by .md or part of plan-otp.md)
    text = re.sub(r'plan-otp-(\d\d)(?!\.)', repl_bare, text)
    return text


def fix_context_refs(text):
    """Fix **Context:** line references that use filenames."""
    def repl(m):
        num = m.group(1)
        title = m.group(2)
        return f'`TOTP {num}`'
    # Don't change plan-otp.md references in context lines
    return text


def process_file(filepath):
    with open(filepath) as f:
        text = f.read()
    original = text

    # Step 1: Join paragraph lines and list continuations
    text = join_paragraphs_and_continuations(text)

    # Step 2: Fix metadata (assignedTo, links)
    text = fix_metadata(text)

    # Step 3: Fix references (plan-otp-NN-xxx.md -> TOTP NN)
    text = fix_references(text)

    if text != original:
        with open(filepath, 'w') as f:
            f.write(text)
        return True
    return False


def main():
    files = sorted(glob.glob('plan-otp-*.md'))
    changed = []
    for f in files:
        if process_file(f):
            changed.append(f)
    print(f"Processed {len(files)} files, {len(changed)} changed:")
    for f in changed:
        print(f"  {f}")


if __name__ == '__main__':
    main()