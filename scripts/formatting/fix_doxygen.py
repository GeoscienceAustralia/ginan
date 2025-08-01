#!/usr/bin/env python3

import re
import sys
import os

def fix_doxygen_comments(content):
    """
    Fix Doxygen comments that appear after function parameter closing parenthesis
    by moving them to the end of the last parameter line.
    """
    
    # Pattern to match function declarations with trailing Doxygen comments
    # This matches the last parameter line followed by closing paren and comment
    pattern = r'(\n\s*)([^)\n]+)(\s*\n\s*\)\s*)(///< [^\n\r]*)'
    
    def replace_func(match):
        indent = match.group(1)
        last_param = match.group(2)
        closing_section = match.group(3)
        comment = match.group(4).strip()
        
        # Add comment to end of last parameter line
        return f'{indent}{last_param}  {comment}{closing_section}'
    
    # Apply the replacement
    result = re.sub(pattern, replace_func, content)
    
    return result

def process_file(filepath):
    """Process a single file to fix Doxygen comments."""
    try:
        with open(filepath, 'r', encoding='utf-8') as f:
            content = f.read()
        
        original_content = content
        fixed_content = fix_doxygen_comments(content)
        
        if fixed_content != original_content:
            with open(filepath, 'w', encoding='utf-8') as f:
                f.write(fixed_content)
            print(f"Fixed: {filepath}")
            return True
        else:
            print(f"No changes needed: {filepath}")
            return False
            
    except Exception as e:
        print(f"Error processing {filepath}: {e}")
        return False

def main():
    if len(sys.argv) < 2:
        print("Usage: python fix_doxygen.py <file_or_directory>")
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
