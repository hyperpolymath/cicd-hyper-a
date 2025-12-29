# Security Audit Summary - 2025-12-29

## Dependabot Vulnerabilities (3 repos - ALL FIXED)

| Repo | Package | CVE | Severity | Fix |
|------|---------|-----|----------|-----|
| disinfo-nesy-detector | protobuf | CVE-2025-53605 | Medium | Added protobuf = "3.7" |
| indieweb2-bastion | protobuf | CVE-2025-53605 | Medium | Added protobuf = "3.7" |
| personal-sysadmin | ring | CVE-2025-4432 | Medium | Added ring = "0.17.13" |

## Code Scanning Alerts (233 repos)

Primary issue: `actions/missing-workflow-permissions`
- Workflows need `permissions: read-all` at workflow level
- Fix: Added via GitHub API batch update

## Root Causes

1. **Transitive Dependencies**: prometheus crate pulls in old protobuf
2. **Template Propagation**: Workflow templates missing permissions declaration
3. **Incremental Drift**: New workflows added without security review

## Prevention Recommendations

1. Add Dependabot auto-merge for patch updates
2. Add workflow linter to CI
3. Use renovate bot for better dependency management
4. Pre-commit hooks for workflow validation
