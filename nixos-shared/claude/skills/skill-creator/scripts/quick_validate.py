#! /usr/bin/env nix
#! nix shell --impure --expr ``
#! nix with (import (builtins.getFlake ''nixpkgs'') {});
#! nix python3.withPackages (ps: with ps; [ pyyaml ])
#! nix ``
#! nix --command python3
"""
Quick validation script for skills - checks structure and best practices
"""

import sys
import os
import re
import yaml
from pathlib import Path

# Reserved words that cannot appear in skill names
RESERVED_WORDS = {'anthropic', 'claude'}

def validate_skill(skill_path):
    """Validate a skill against best practices"""
    skill_path = Path(skill_path)
    errors = []
    warnings = []

    # Check SKILL.md exists
    skill_md = skill_path / 'SKILL.md'
    if not skill_md.exists():
        return False, "SKILL.md not found"

    content = skill_md.read_text()

    # Check frontmatter exists
    if not content.startswith('---'):
        return False, "No YAML frontmatter found"

    # Extract frontmatter
    match = re.match(r'^---\n(.*?)\n---', content, re.DOTALL)
    if not match:
        return False, "Invalid frontmatter format"

    frontmatter_text = match.group(1)
    body = content[match.end():]

    # Parse YAML frontmatter
    try:
        frontmatter = yaml.safe_load(frontmatter_text)
        if not isinstance(frontmatter, dict):
            return False, "Frontmatter must be a YAML dictionary"
    except yaml.YAMLError as e:
        return False, f"Invalid YAML in frontmatter: {e}"

    # Define allowed properties
    ALLOWED_PROPERTIES = {'name', 'description', 'license', 'allowed-tools', 'metadata'}

    # Check for unexpected properties
    unexpected_keys = set(frontmatter.keys()) - ALLOWED_PROPERTIES
    if unexpected_keys:
        return False, (
            f"Unexpected key(s) in SKILL.md frontmatter: {', '.join(sorted(unexpected_keys))}. "
            f"Allowed properties are: {', '.join(sorted(ALLOWED_PROPERTIES))}"
        )

    # Validate name
    if 'name' not in frontmatter:
        return False, "Missing 'name' in frontmatter"

    name = frontmatter.get('name', '')
    if not isinstance(name, str):
        return False, f"Name must be a string, got {type(name).__name__}"
    name = name.strip()

    if name:
        # Check naming convention (hyphen-case)
        if not re.match(r'^[a-z0-9-]+$', name):
            return False, f"Name '{name}' should be hyphen-case (lowercase letters, digits, and hyphens only)"
        if name.startswith('-') or name.endswith('-') or '--' in name:
            return False, f"Name '{name}' cannot start/end with hyphen or contain consecutive hyphens"
        # Check name length (max 64 characters per spec)
        if len(name) > 64:
            return False, f"Name is too long ({len(name)} characters). Maximum is 64 characters."
        # Check for reserved words
        for reserved in RESERVED_WORDS:
            if reserved in name.lower():
                return False, f"Name '{name}' contains reserved word '{reserved}'"

    # Validate description
    if 'description' not in frontmatter:
        return False, "Missing 'description' in frontmatter"

    description = frontmatter.get('description', '')
    if not isinstance(description, str):
        return False, f"Description must be a string, got {type(description).__name__}"
    description = description.strip()

    if not description:
        return False, "Description cannot be empty"

    if description:
        # Check for angle brackets (XML tags)
        if '<' in description or '>' in description:
            return False, "Description cannot contain angle brackets (< or >)"
        # Check description length (max 1024 characters per spec)
        if len(description) > 1024:
            return False, f"Description is too long ({len(description)} characters). Maximum is 1024 characters."

        # Check for first/second person (warnings)
        first_person = re.search(r'\b(I can|I will|I help|I\'m|I am)\b', description, re.IGNORECASE)
        if first_person:
            warnings.append(f"Description uses first person ('{first_person.group()}'). Use third person instead.")

        second_person = re.search(r'\b(You can|You will|your)\b', description, re.IGNORECASE)
        if second_person:
            warnings.append(f"Description uses second person ('{second_person.group()}'). Use third person instead.")

        # Check for vague descriptions
        vague_patterns = [
            r'^helps?\s+(with|you)',
            r'^processes?\s+data$',
            r'^does\s+stuff',
            r'^useful\s+for',
        ]
        for pattern in vague_patterns:
            if re.search(pattern, description, re.IGNORECASE):
                warnings.append("Description appears vague. Include specific triggers and use cases.")
                break

    # Validate body
    body_lines = body.strip().split('\n')
    if len(body_lines) > 500:
        warnings.append(f"SKILL.md body has {len(body_lines)} lines. Recommended maximum is 500 lines.")

    # Check for TODO placeholders
    if '[TODO' in content or 'TODO:' in content:
        warnings.append("SKILL.md contains TODO placeholders that should be completed.")

    # Check for Windows-style paths
    if re.search(r'\\[a-zA-Z]', content):
        warnings.append("Possible Windows-style paths detected. Use forward slashes for cross-platform compatibility.")

    # Build result message
    if errors:
        return False, "; ".join(errors)

    if warnings:
        return True, "Skill is valid with warnings:\n  - " + "\n  - ".join(warnings)

    return True, "Skill is valid!"

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python quick_validate.py <skill_directory>")
        sys.exit(1)

    valid, message = validate_skill(sys.argv[1])
    print(message)
    sys.exit(0 if valid else 1)
