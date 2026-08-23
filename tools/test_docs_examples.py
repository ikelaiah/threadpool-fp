#!/usr/bin/env python3
"""Compile and run the Pascal programs shown in the documentation recipes."""

from __future__ import annotations

import argparse
import shutil
import subprocess
import sys
from pathlib import Path


def normalise(text: str) -> str:
    return text.replace("\r\n", "\n").rstrip("\n")


def run(command: list[str], cwd: Path) -> subprocess.CompletedProcess[str]:
    return subprocess.run(command, cwd=cwd, text=True, capture_output=True, check=False)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--fpc", default="fpc", help="Free Pascal Compiler command")
    parser.add_argument("--root", type=Path, default=Path(__file__).resolve().parents[1])
    parser.add_argument("--build-dir", type=Path, default=Path("build/docs-examples"))
    args = parser.parse_args()

    root = args.root.resolve()
    build_dir = (root / args.build_dir).resolve()
    units = build_dir / "units"
    binaries = build_dir / "bin"
    if build_dir.exists():
        shutil.rmtree(build_dir)
    units.mkdir(parents=True)
    binaries.mkdir()

    examples = sorted((root / "examples" / "documentation").glob("*.pas"))
    if not examples:
        print("No documentation examples found.", file=sys.stderr)
        return 1

    failures: list[str] = []
    for source in examples:
        expected_path = source.with_suffix(".output")
        if not expected_path.is_file():
            failures.append(f"{source}: missing expected output file")
            continue
        compile_result = run(
            [
                args.fpc,
                "-MObjFPC",
                "-Scghi",
                f"-Fu{root / 'src'}",
                f"-FU{units}",
                f"-FE{binaries}",
                str(source),
            ],
            root,
        )
        if compile_result.returncode:
            failures.append(f"{source}: compilation failed\n{compile_result.stdout}{compile_result.stderr}")
            continue
        executable = binaries / source.stem
        if sys.platform == "win32":
            executable = executable.with_suffix(".exe")
        output_result = run([str(executable)], root)
        expected = normalise(expected_path.read_text(encoding="utf-8"))
        actual = normalise(output_result.stdout)
        if output_result.returncode or actual != expected:
            failures.append(
                f"{source}: output mismatch\nexpected:\n{expected}\nactual:\n{actual}\n{output_result.stderr}"
            )

    if failures:
        print("\n\n".join(failures), file=sys.stderr)
        return 1
    print(f"Documentation examples passed: {len(examples)} compiled and run with expected output")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())