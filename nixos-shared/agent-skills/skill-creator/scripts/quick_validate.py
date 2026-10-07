#! /usr/bin/env nix
#! nix shell --impure --expr ``
#! nix with (import (builtins.getFlake ''nixpkgs'') {});
#! nix python3.withPackages (ps: with ps; [ pyyaml ])
#! nix ``
#! nix --command python3
"""
Quick validation script for skills - checks structure and best practices.
Prints ERROR/WARNING lines; exits 1 if there are errors, 0 otherwise.
"""

import sys
import re
import yaml
from pathlib import Path

# Reserved words that cannot appear in skill names
RESERVED_WORDS = {'anthropic', 'claude'}


def validate_skill(skill_path):
    """Return (errors, warnings) for a skill directory."""
    skill_path = Path(skill_path)
    errors = []
    warnings = []

    skill_md = skill_path / 'SKILL.md'
    if not skill_md.exists():
        return ["SKILL.md not found"], warnings

    content = skill_md.read_text()
    match = re.match(r'^---\n(.*?)\n---', content, re.DOTALL)
    if not match:
        return ["No valid YAML frontmatter found"], warnings
    body = content[match.end():]

    try:
        frontmatter = yaml.safe_load(match.group(1))
    except yaml.YAMLError as e:
        return [f"Invalid YAML in frontmatter: {e}"], warnings
    if not isinstance(frontmatter, dict):
        return ["Frontmatter must be a YAML dictionary"], warnings

    allowed = {'name', 'description', 'license', 'allowed-tools', 'metadata', 'compatibility',
               # Claude Code extensions; pi ignores keys it does not know.
               'context', 'agent', 'model', 'argument-hint', 'user-invocable',
               'disable-model-invocation', 'hooks'}
    unexpected = set(frontmatter) - allowed
    if unexpected:
        errors.append(
            f"Unexpected frontmatter key(s): {', '.join(sorted(unexpected))}. "
            f"Allowed: {', '.join(sorted(allowed))}"
        )

    # name
    name = frontmatter.get('name')
    if name is None:
        errors.append("Missing 'name' in frontmatter")
    elif not isinstance(name, str):
        errors.append(f"Name must be a string, got {type(name).__name__}")
    else:
        name = name.strip()
        if not name:
            errors.append("Name cannot be empty")
        else:
            if not re.match(r'^[a-z0-9-]+$', name):
                errors.append(f"Name '{name}' should be hyphen-case (lowercase letters, digits, hyphens)")
            if name.startswith('-') or name.endswith('-') or '--' in name:
                errors.append(f"Name '{name}' cannot start/end with hyphen or contain consecutive hyphens")
            if len(name) > 64:
                errors.append(f"Name is too long ({len(name)} characters). Maximum is 64.")
            for reserved in sorted(RESERVED_WORDS):
                if reserved in name.lower():
                    errors.append(f"Name '{name}' contains reserved word '{reserved}'")

    # description
    description = frontmatter.get('description')
    if description is None:
        errors.append("Missing 'description' in frontmatter")
    elif not isinstance(description, str):
        errors.append(f"Description must be a string, got {type(description).__name__}")
    elif not description.strip():
        errors.append("Description cannot be empty")
    else:
        description = description.strip()
        if '<' in description or '>' in description:
            errors.append("Description cannot contain angle brackets (< or >)")
        if len(description) > 1024:
            errors.append(f"Description is too long ({len(description)} characters). Maximum is 1024.")

        # Third person is the rule for the description only; the body may address the reader.
        person = re.search(r"\b(I can|I will|I help|I'm|I am|You can|You will)\b", description, re.IGNORECASE)
        if person:
            warnings.append(f"Description uses first/second person ('{person.group()}'). Use third person.")

        vague_patterns = [
            r'^helps?\s+(with|you)',
            r'^processes?\s+data$',
            r'^does\s+stuff',
            r'^useful\s+for',
        ]
        if any(re.search(p, description, re.IGNORECASE) for p in vague_patterns):
            warnings.append("Description appears vague. Include specific triggers and use cases.")

    compatibility = frontmatter.get('compatibility')
    if compatibility:
        if not isinstance(compatibility, str):
            errors.append(f"Compatibility must be a string, got {type(compatibility).__name__}")
        elif len(compatibility) > 500:
            errors.append(f"Compatibility is too long ({len(compatibility)} characters). Maximum is 500.")

    # body
    body_lines = body.strip().split('\n')
    if len(body_lines) > 500:
        warnings.append(f"SKILL.md body has {len(body_lines)} lines. Recommended maximum is 500.")
    if '[TODO' in content or 'TODO:' in content:
        warnings.append("SKILL.md contains TODO placeholders that should be completed.")
    if re.search(r'\\[a-zA-Z]', content):
        warnings.append("Possible Windows-style paths detected. Use forward slashes.")

    return errors, warnings


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: quick_validate.py <skill_directory>")
        sys.exit(2)

    errs, warns = validate_skill(sys.argv[1])
    for e in errs:
        print(f"ERROR: {e}")
    for w in warns:
        print(f"WARNING: {w}")
    if not errs and not warns:
        print("Skill is valid!")
    sys.exit(1 if errs else 0)
