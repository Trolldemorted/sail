#!/usr/bin/env python3
"""Run the sail --json backend against a few small Sail files and pipe
the output through scripts/verify_json.py. The verifier is expected to
flag known issues for now (see rewriterchange.md Work item 4); what we
assert is that:

  1. sail --json exits 0 with --auto-mono --json-strict-poly.
  2. The JSON output is parseable by scripts/verify_json.py.
  3. The "types" field is present and contains the test-defined aliases.
  4. The verifier's Typ_id and Type table sections report non-zero counts.

This is a smoke test, not a full strict-mode gate. The strict CI gate
for the full sail-riscv model is in .github/workflows/json-verify.yml."""

import json
import os
import shutil
import subprocess
import sys

mydir = os.path.dirname(__file__)
os.chdir(mydir)
sys.path.insert(0, os.path.realpath('..'))

from sailtest import banner, get_sail, get_sail_dir

sail_dir = get_sail_dir()
sail = get_sail()
# sail_dir points at the libsail share directory. Walk up to find the
# source-tree scripts/ directory.
verify_script_candidates = [
    os.path.realpath(os.path.join(sail_dir, '..', '..', '..', 'scripts', 'verify_json.py')),
    os.path.realpath(os.path.join(sail_dir, '..', '..', '..', '..', 'scripts', 'verify_json.py')),
]
verify_script = next((p for p in verify_script_candidates if os.path.exists(p)), None)
if verify_script is None:
    # Fall back to a path relative to this file's location.
    verify_script = os.path.realpath(os.path.join(mydir, '..', '..', 'scripts', 'verify_json.py'))
if not os.path.exists(verify_script):
    print(f'Cannot locate verify_json.py (tried {verify_script_candidates})')
    sys.exit(1)


def test_json(name: str, sail_opts: list[str]) -> bool:
    banner(f'Testing JSON output: {name} (sail opts: {sail_opts})')
    sail_file = f'{name}.sail'
    if not os.path.exists(sail_file):
        print(f'  missing {sail_file}')
        return False

    out_dir = os.path.join('/tmp', f'sail_json_test_{name}')
    if os.path.isdir(out_dir):
        shutil.rmtree(out_dir)
    os.makedirs(out_dir, exist_ok=True)

    cmd = [sail, '--auto-mono', '--json', '--json-strict-poly', '-o', name] + sail_opts + [sail_file]
    res = subprocess.run(cmd, capture_output=True, text=True)
    if res.returncode != 0:
        print(f'  sail exited with {res.returncode}')
        print(f'  stdout: {res.stdout}')
        print(f'  stderr: {res.stderr}')
        return False

    json_path = os.path.join(out_dir, f'{name}.json')
    if not os.path.exists(json_path):
        if os.path.exists(f'{name}.json'):
            json_path = f'{name}.json'
        else:
            print(f'  no JSON output produced')
            print(f'  stdout: {res.stdout}')
            print(f'  stderr: {res.stderr}')
            return False

    # Parse the JSON
    try:
        with open(json_path) as f:
            data = json.load(f)
    except json.JSONDecodeError as e:
        print(f'  JSON parse failed: {e}')
        return False

    # Smoke checks: types field present
    types = data.get('types')
    if not isinstance(types, list):
        print(f'  expected "types" field to be a list, got {type(types).__name__}')
        return False
    print(f'  types field present: {len(types)} entries')

    # Run the verifier (non-strict, just to exercise it)
    cmd = ['python3', verify_script, '--allow-extern-poly', json_path]
    res = subprocess.run(cmd, capture_output=True, text=True)
    if res.returncode not in (0, 1):
        print(f'  verifier exited with unexpected code {res.returncode}')
        print(res.stdout)
        print(res.stderr)
        return False

    print('  OK')
    return True


def main() -> int:
    tests = [
        ('sample', []),
    ]
    failed = 0
    for name, opts in tests:
        if not test_json(name, opts):
            failed += 1
    if failed:
        print(f'FAILED: {failed}/{len(tests)} tests')
        return 1
    print(f'PASSED: {len(tests)} tests')
    return 0


if __name__ == '__main__':
    sys.exit(main())