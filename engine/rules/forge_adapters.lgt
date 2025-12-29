%% SPDX-License-Identifier: AGPL-3.0-or-later
%% Forge Adapters - Multi-forge support

:- object(forge_adapters).

    :- info([
        version is 1:'0':'0',
        author is 'cicd-hyper-a',
        date is 2025-12-29,
        comment is 'Adapters for GitHub, GitLab, Bitbucket, etc.'
    ]).

    :- public([
        supported_forge/1,
        get_repos/2,
        get_alerts/3,
        deploy_workflow/4,
        enable_branch_protection/2,
        create_pr/4
    ]).

    %% ============================================================
    %% SUPPORTED FORGES
    %% ============================================================

    supported_forge(github).
    supported_forge(gitlab).
    supported_forge(bitbucket).
    supported_forge(codeberg).
    supported_forge(sourcehut).
    supported_forge(gitea).
    supported_forge(radicle).

    %% ============================================================
    %% FORGE OPERATIONS (Interface)
    %% ============================================================

    %% Get all repos for an org
    get_repos(github, Org) :-
        format(atom(Cmd), 'gh repo list ~w --limit 500 --json name', [Org]),
        shell(Cmd).

    get_repos(gitlab, Org) :-
        format(atom(Cmd), 'glab repo list -g ~w --per-page 100', [Org]),
        shell(Cmd).

    get_repos(bitbucket, Org) :-
        format(atom(Cmd), 'curl -s "https://api.bitbucket.org/2.0/repositories/~w"', [Org]),
        shell(Cmd).

    %% Get security alerts
    get_alerts(github, Org, Repo) :-
        format(atom(Cmd), 'gh api repos/~w/~w/code-scanning/alerts', [Org, Repo]),
        shell(Cmd).

    get_alerts(gitlab, Org, Repo) :-
        format(atom(Cmd), 'glab api projects/~w%2F~w/vulnerability_findings', [Org, Repo]),
        shell(Cmd).

    %% Deploy workflow
    deploy_workflow(github, Org, Repo, Workflow) :-
        format(atom(Cmd),
            'gh api repos/~w/~w/contents/.github/workflows/~w -X PUT -f message="Add ~w" -f content="$(base64 < ~w)"',
            [Org, Repo, Workflow, Workflow, Workflow]),
        shell(Cmd).

    %% Enable branch protection
    enable_branch_protection(github, Repo) :-
        format(atom(Cmd),
            'gh api repos/hyperpolymath/~w/branches/main/protection -X PUT -f required_pull_request_reviews[required_approving_review_count]=1',
            [Repo]),
        shell(Cmd).

    %% Create PR
    create_pr(github, Repo, Title, Body) :-
        format(atom(Cmd),
            'gh pr create -R hyperpolymath/~w --title "~w" --body "~w"',
            [Repo, Title, Body]),
        shell(Cmd).

    create_pr(gitlab, Repo, Title, Body) :-
        format(atom(Cmd),
            'glab mr create -R hyperpolymath/~w --title "~w" --description "~w"',
            [Repo, Title, Body]),
        shell(Cmd).

:- end_object.
