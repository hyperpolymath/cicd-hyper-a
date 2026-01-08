%% SPDX-License-Identifier: AGPL-3.0-or-later
%% cicd-hyper-a Training Data for Logtalk Learning Module
%% Format: error(Id, Category, Severity, AutoFix, Description, FixPattern)
%% Auto-generated from ERROR-CATALOG.scm

%% ===== Workflow Security =====
error('TokenPermissionsID', workflow_security, high, true,
      'Workflow missing explicit permissions declaration',
      'Add permissions: read-all at workflow level').

error('PinnedDependenciesID', workflow_security, medium, true,
      'GitHub Actions not SHA-pinned',
      'Replace @v4 with @SHA # v4 format').

error(workflow_linter_self_detection, workflow_security, low, true,
      'Workflow linter grep matches its own comments',
      'Add grep -v filters for comment lines').

error(missing_action_input, workflow_security, medium, true,
      'GitHub Action missing required input parameter',
      'Add with: section with required inputs').

error(codeql_language_mismatch, workflow_security, medium, true,
      'CodeQL configured for languages not present in repository',
      'Update language matrix to match repo contents; use actions for workflow scanning').

error(missing_workflow_permissions, workflow_security, high, true,
      'Workflow does not contain permissions',
      'Add permissions: read-all at workflow level').

%% ===== Code Security =====
error(hard_coded_cryptographic_value, code_security, critical, false,
      'Hard-coded secret, key, or token in source code',
      'Use environment variables or secrets manager').

error(remote_property_injection, code_security, high, false,
      'Dynamic property access without validation',
      'Add allowlist validation for property names').

error(unused_local_variable, code_quality, low, true,
      'Unused variable, import, function or class',
      'Remove unused code or prefix with underscore').

error(syntax_error, code_quality, medium, false,
      'JavaScript/TypeScript syntax error',
      'Fix syntax error in source file').

%% ===== Dependency Vulnerabilities =====
error('VulnerabilitiesID', dependency_vuln, high, false,
      'Known vulnerabilities in dependencies',
      'Run cargo audit / npm audit and update deps').

error(unmaintained_crate, dependency_vuln, medium, false,
      'Dependency is unmaintained',
      'Find alternative or fork and maintain').

%% ===== Process Hygiene =====
error('SecurityPolicyID', process_hygiene, medium, true,
      'Missing SECURITY.md file',
      'Add SECURITY.md with reporting instructions').

error('MaintainedID', process_hygiene, low, false,
      'Repository shows low activity',
      'Organic commit activity - cannot automate').

error('CodeReviewID', process_hygiene, medium, true,
      'Pull requests merged without review',
      'Enable branch protection with required reviews').

error('BranchProtectionID', process_hygiene, medium, true,
      'Branch protection not enabled',
      'Enable via GitHub API or UI').

error('CIIBestPracticesID', process_hygiene, low, false,
      'Not registered for CII Best Practices badge',
      'Register at bestpractices.coreinfrastructure.org').

%% ===== Testing & Analysis =====
error('FuzzingID', missing_fuzzing, medium, true,
      'No fuzzing infrastructure configured',
      'Add ClusterFuzzLite or cargo-fuzz setup').

error('SASTID', missing_sast, medium, true,
      'No static analysis configured',
      'Add CodeQL workflow with correct language matrix').

error('CITestsID', missing_tests, medium, true,
      'No automated test workflow detected',
      'Add test workflow for project language').

%% ===== RSR Language Policy =====
error(typescript_detected, language_policy, high, false,
      'TypeScript files detected - RSR policy violation',
      'Convert to ReScript').

error(golang_detected, language_policy, high, false,
      'Go files detected - RSR policy violation',
      'Rewrite in Rust').

error(python_detected, language_policy, high, false,
      'Python files detected outside SaltStack - RSR policy violation',
      'Rewrite in Julia, Rust, or ReScript').

error(nodejs_detected, language_policy, high, false,
      'Node.js artifacts detected - RSR policy violation',
      'Migrate to Deno').

error(makefile_detected, language_policy, medium, false,
      'Makefile detected - RSR policy violation',
      'Migrate to Just, Guix, or Nix').

%% ===== SPDX/License =====
error(missing_spdx_header, license_compliance, low, true,
      'File missing SPDX license header',
      'Add SPDX-License-Identifier comment as first line').
