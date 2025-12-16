#!/usr/bin/env python3

import re
import sys
import os

def reorganize_includes_and_using(content):
    """
    Reorganize C++ file to have: includes → namespace aliases → using statements.
    Handles template aliases correctly.
    """

    # Split content into lines for easier processing
    lines = content.split('\n')

    # Lists to collect different types of lines
    header_comments = []
    includes = []
    namespace_aliases = []
    using_statements = []
    other_lines = []

    # Track what section we're in
    found_first_include = False
    found_first_namespace = False
    found_first_using = False
    collecting_includes_and_using = True

    i = 0
    while i < len(lines):
        line = lines[i].strip()

        # Skip empty lines when collecting
        if not line and collecting_includes_and_using:
            i += 1
            continue

        # Check for include statements
        if line.startswith('#include'):
            if not found_first_include:
                found_first_include = True
            includes.append(lines[i])  # Keep original formatting
            i += 1
            continue

        # Check for namespace aliases (namespace alias = existing::namespace)
        if line.startswith('namespace ') and '=' in line:
            if not found_first_namespace:
                found_first_namespace = True
            namespace_aliases.append(lines[i])  # Keep original formatting
            i += 1
            continue

        # Check for using statements (but not template aliases)
        if line.startswith('using '):
            # Check if this is a template alias by looking for 'template' on same or previous lines
            is_template_alias = False

            # Check current line for template
            if 'template' in line:
                is_template_alias = True

            # Check if previous non-empty line contains template
            j = i - 1
            while j >= 0 and not lines[j].strip():
                j -= 1
            if j >= 0 and 'template' in lines[j]:
                is_template_alias = True

            if not is_template_alias:
                if not found_first_using:
                    found_first_using = True
                using_statements.append(lines[i])  # Keep original formatting
                i += 1
                continue

        # If we haven't found includes/namespace/using yet, it's probably header comments
        if not found_first_include and not found_first_namespace and not found_first_using:
            header_comments.append(lines[i])
            i += 1
            continue

        # If we've found includes/namespace/using but this line is neither, we're done collecting
        if found_first_include or found_first_namespace or found_first_using:
            collecting_includes_and_using = False

        # Everything else goes to other_lines
        other_lines.append(lines[i])
        i += 1

    # Reconstruct the file
    result_lines = []

    # Add header comments
    result_lines.extend(header_comments)

    # Add a blank line if we have header comments and includes
    if header_comments and includes:
        result_lines.append('')

    # Add all includes
    result_lines.extend(includes)

    # Add a blank line between includes and namespace aliases
    if includes and namespace_aliases:
        result_lines.append('')

    # Add all namespace aliases
    result_lines.extend(namespace_aliases)

    # Add a blank line between namespace aliases and using statements
    if namespace_aliases and using_statements:
        result_lines.append('')
    # Add a blank line between includes and using statements when no namespace aliases
    elif includes and using_statements and not namespace_aliases:
        result_lines.append('')
    # Add all using statements
    result_lines.extend(using_statements)

    # Add a blank line between using statements and other code
    if using_statements and other_lines:
        result_lines.append('')

    # Add the rest of the code
    result_lines.extend(other_lines)

    return '\n'.join(result_lines)

def process_file(filepath):
    """Process a single file to reorganize includes and using statements."""
    try:
        with open(filepath, 'r', encoding='utf-8') as f:
            content = f.read()

        original_content = content
        reorganized_content = reorganize_includes_and_using(content)

        if reorganized_content != original_content:
            with open(filepath, 'w', encoding='utf-8') as f:
                f.write(reorganized_content)
            print(f"Reorganized: {filepath}")
            return True
        else:
            print(f"No changes needed: {filepath}")
            return False

    except Exception as e:
        print(f"Error processing {filepath}: {e}")
        return False

def main():
    if len(sys.argv) < 2:
        print("Usage: python reorganize_includes.py <file_or_directory>")
        sys.exit(1)

    target = sys.argv[1]

    if os.path.isfile(target):
        # Process single file
        process_file(target)
    elif os.path.isdir(target):
        # Process all .cpp and .hpp files in directory recursively
        files_processed = 0
        files_changed = 0

        for root, dirs, files in os.walk(target):
            for file in files:
                if file.endswith(('.cpp', '.hpp', '.h', '.cc', '.cxx')):
                    filepath = os.path.join(root, file)
                    files_processed += 1
                    if process_file(filepath):
                        files_changed += 1

        print(f"\nSummary: {files_changed} files changed out of {files_processed} processed")
    else:
        print(f"Error: {target} is not a valid file or directory")
        sys.exit(1)

if __name__ == "__main__":
    main()