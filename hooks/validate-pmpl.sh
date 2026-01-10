#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0
# Pre-commit/pre-push hook: Validate PMPL license and badge presence

set -euo pipefail

ERRORS=0

LICENSE_FILES=("LICENSE" "LICENSE.txt")
README_FILES=("README.adoc" "README.md" "README.rst" "README.txt" "README")

LICENSE_OK=0
for file in "${LICENSE_FILES[@]}"; do
  if [ -f "$file" ]; then
    if grep -qE "PMPL-1.0|Palimpsest-MPL License v1.0" "$file"; then
      LICENSE_OK=1
      break
    fi
  fi
done

if [ "$LICENSE_OK" -eq 0 ]; then
  echo "ERROR: Missing PMPL license file or PMPL content."
  echo "  Expected LICENSE or LICENSE.txt containing PMPL-1.0."
  ERRORS=$((ERRORS + 1))
fi

README_OK=0
for file in "${README_FILES[@]}"; do
  if [ -f "$file" ]; then
    if grep -q "img.shields.io/badge/license-PMPL--1.0" "$file"; then
      README_OK=1
      break
    fi
  fi
done

if [ "$README_OK" -eq 0 ]; then
  echo "ERROR: Missing PMPL badge in README."
  echo "  Expected badge URL: https://img.shields.io/badge/license-PMPL--1.0-blue.svg"
  ERRORS=$((ERRORS + 1))
fi

if [ $ERRORS -gt 0 ]; then
  exit 1
fi

exit 0
