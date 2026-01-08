%% SPDX-License-Identifier: AGPL-3.0-or-later
%% Learning Module - Continuous learning from audit data and feedback

:- object(learning).

    :- info([
        version is 1:'0':'0',
        author is 'cicd-hyper-a',
        date is 2025-01-08,
        comment is 'Continuous learning from audit data, user feedback, and fix outcomes'
    ]).

    :- public([
        %% Core learning operations
        learn_from_fix/3,
        learn_from_alert/4,
        learn_from_feedback/3,
        learn_pattern/4,

        %% Knowledge retrieval
        get_learned_rules/1,
        get_confidence/2,
        get_success_rate/2,
        recommend_fix/2,

        %% Persistence
        save_knowledge/1,
        load_knowledge/1,
        export_as_rules/1,

        %% Learning statistics
        get_learning_stats/1,
        prune_low_confidence/1
    ]).

    :- private([
        learned_fix/5,           % learned_fix(IssueType, Fix, Confidence, SuccessCount, FailCount)
        learned_pattern/4,       % learned_pattern(Pattern, Category, Severity, Count)
        feedback_record/4,       % feedback_record(IssueType, Fix, Outcome, Timestamp)
        knowledge_version/1
    ]).

    :- dynamic([
        learned_fix/5,
        learned_pattern/4,
        feedback_record/4,
        knowledge_version/1
    ]).

    %% ============================================================
    %% CORE LEARNING OPERATIONS
    %% ============================================================

    %% Learn from a successful or failed fix attempt
    %% Outcome: success | failure | partial
    learn_from_fix(IssueType, Fix, Outcome) :-
        get_time(Timestamp),
        assertz(feedback_record(IssueType, Fix, Outcome, Timestamp)),
        update_fix_confidence(IssueType, Fix, Outcome).

    update_fix_confidence(IssueType, Fix, success) :-
        ( learned_fix(IssueType, Fix, Conf, Succ, Fail) ->
            retract(learned_fix(IssueType, Fix, Conf, Succ, Fail)),
            Succ1 is Succ + 1,
            Total is Succ1 + Fail,
            NewConf is Succ1 / Total,
            assertz(learned_fix(IssueType, Fix, NewConf, Succ1, Fail))
        ; assertz(learned_fix(IssueType, Fix, 1.0, 1, 0))
        ).

    update_fix_confidence(IssueType, Fix, failure) :-
        ( learned_fix(IssueType, Fix, Conf, Succ, Fail) ->
            retract(learned_fix(IssueType, Fix, Conf, Succ, Fail)),
            Fail1 is Fail + 1,
            Total is Succ + Fail1,
            NewConf is Succ / Total,
            assertz(learned_fix(IssueType, Fix, NewConf, Succ, Fail1))
        ; assertz(learned_fix(IssueType, Fix, 0.0, 0, 1))
        ).

    update_fix_confidence(IssueType, Fix, partial) :-
        ( learned_fix(IssueType, Fix, Conf, Succ, Fail) ->
            retract(learned_fix(IssueType, Fix, Conf, Succ, Fail)),
            % Partial success counts as 0.5
            Succ1 is Succ + 0.5,
            Fail1 is Fail + 0.5,
            Total is Succ1 + Fail1,
            NewConf is Succ1 / Total,
            assertz(learned_fix(IssueType, Fix, NewConf, Succ1, Fail1))
        ; assertz(learned_fix(IssueType, Fix, 0.5, 0.5, 0.5))
        ).

    %% Learn from a new alert type
    learn_from_alert(AlertId, Category, Severity, Description) :-
        ( learned_pattern(AlertId, _, _, Count) ->
            retract(learned_pattern(AlertId, _, _, Count)),
            Count1 is Count + 1,
            assertz(learned_pattern(AlertId, Category, Severity, Count1))
        ; assertz(learned_pattern(AlertId, Category, Severity, 1))
        ),
        % Try to auto-derive fix from cicd_rules
        ( cicd_rules::suggest_fix(AlertId, _) ->
            true
        ; derive_fix_suggestion(AlertId, Category, Description)
        ).

    derive_fix_suggestion(_, _, _) :- true.  % Placeholder for ML-based derivation

    %% Learn from explicit user feedback
    learn_from_feedback(IssueType, Feedback, Context) :-
        get_time(Timestamp),
        assertz(feedback_record(IssueType, Feedback, user_feedback, Timestamp)),
        process_feedback(IssueType, Feedback, Context).

    process_feedback(IssueType, fix_worked, _) :-
        ( learned_fix(IssueType, _, Conf, Succ, Fail), Conf >= 0.5 ->
            true  % Already tracked
        ; true    % No action needed
        ).
    process_feedback(IssueType, fix_failed, Context) :-
        % Record that the suggested fix didn't work in this context
        assertz(feedback_record(IssueType, context(Context), failure, _)).
    process_feedback(_, _, _) :- true.

    %% Learn a new pattern (from external analysis)
    learn_pattern(Pattern, Category, Severity, Fix) :-
        ( learned_pattern(Pattern, _, _, _) ->
            true  % Already known
        ; assertz(learned_pattern(Pattern, Category, Severity, 1)),
          ( nonvar(Fix) ->
              assertz(learned_fix(Pattern, Fix, 0.8, 4, 1))  % Initial confidence from training
          ; true
          )
        ).

    %% ============================================================
    %% KNOWLEDGE RETRIEVAL
    %% ============================================================

    get_learned_rules(Rules) :-
        findall(
            rule(IssueType, Fix, Confidence),
            (learned_fix(IssueType, Fix, Confidence, _, _), Confidence >= 0.6),
            Rules
        ).

    get_confidence(IssueType, Confidence) :-
        learned_fix(IssueType, _, Confidence, _, _).

    get_success_rate(IssueType, Rate) :-
        findall(
            Succ-Fail,
            learned_fix(IssueType, _, _, Succ, Fail),
            Pairs
        ),
        sum_pairs(Pairs, TotalSucc, TotalFail),
        Total is TotalSucc + TotalFail,
        ( Total > 0 ->
            Rate is TotalSucc / Total
        ; Rate is 0.0
        ).

    sum_pairs([], 0, 0).
    sum_pairs([S-F|Rest], TotalS, TotalF) :-
        sum_pairs(Rest, RestS, RestF),
        TotalS is S + RestS,
        TotalF is F + RestF.

    %% Recommend best fix based on learned confidence
    recommend_fix(IssueType, Fix) :-
        findall(
            Conf-Fix,
            learned_fix(IssueType, Fix, Conf, _, _),
            Pairs
        ),
        sort(0, @>=, Pairs, Sorted),
        ( Sorted = [_-BestFix|_] ->
            Fix = BestFix
        ; cicd_rules::suggest_fix(IssueType, Fix)  % Fallback to static rules
        ).

    %% ============================================================
    %% PERSISTENCE
    %% ============================================================

    save_knowledge(FilePath) :-
        open(FilePath, write, Stream),
        write(Stream, '%% SPDX-License-Identifier: AGPL-3.0-or-later\n'),
        write(Stream, '%% cicd-hyper-a Learned Knowledge Base\n'),
        get_time(Now),
        format(Stream, '%% Generated: ~w~n~n', [Now]),

        % Save learned fixes
        write(Stream, '%% Learned Fixes (IssueType, Fix, Confidence, SuccessCount, FailCount)\n'),
        forall(
            learned_fix(Issue, Fix, Conf, Succ, Fail),
            format(Stream, 'learned_fix(~q, ~q, ~w, ~w, ~w).~n', [Issue, Fix, Conf, Succ, Fail])
        ),

        % Save learned patterns
        write(Stream, '\n%% Learned Patterns (Pattern, Category, Severity, Count)\n'),
        forall(
            learned_pattern(Pattern, Cat, Sev, Count),
            format(Stream, 'learned_pattern(~q, ~q, ~q, ~w).~n', [Pattern, Cat, Sev, Count])
        ),

        close(Stream).

    load_knowledge(FilePath) :-
        ( exists_file(FilePath) ->
            open(FilePath, read, Stream),
            read_knowledge(Stream),
            close(Stream)
        ; true  % File doesn't exist yet, start fresh
        ).

    read_knowledge(Stream) :-
        read(Stream, Term),
        ( Term == end_of_file ->
            true
        ; assert_knowledge(Term),
          read_knowledge(Stream)
        ).

    assert_knowledge(learned_fix(A, B, C, D, E)) :- !,
        assertz(learned_fix(A, B, C, D, E)).
    assert_knowledge(learned_pattern(A, B, C, D)) :- !,
        assertz(learned_pattern(A, B, C, D)).
    assert_knowledge(_).

    %% Export learned knowledge as static rules for cicd_rules
    export_as_rules(FilePath) :-
        open(FilePath, write, Stream),
        write(Stream, '%% SPDX-License-Identifier: AGPL-3.0-or-later\n'),
        write(Stream, '%% Auto-generated rules from learning module\n'),
        write(Stream, '%% Import these into cicd_rules.lgt\n\n'),

        forall(
            (learned_fix(Issue, Fix, Conf, _, _), Conf >= 0.75),
            format(Stream, 'suggest_fix(~q, ~q).  %% confidence: ~2f~n', [Issue, Fix, Conf])
        ),

        forall(
            (learned_pattern(Pattern, _, Severity, Count), Count >= 5),
            format(Stream, 'classify_severity(~q, ~q).  %% seen ~w times~n', [Pattern, Severity, Count])
        ),

        close(Stream).

    %% ============================================================
    %% LEARNING STATISTICS
    %% ============================================================

    get_learning_stats(Stats) :-
        findall(_, learned_fix(_, _, _, _, _), Fixes),
        length(Fixes, FixCount),
        findall(_, learned_pattern(_, _, _, _), Patterns),
        length(Patterns, PatternCount),
        findall(_, feedback_record(_, _, _, _), Feedback),
        length(Feedback, FeedbackCount),
        findall(
            Conf,
            (learned_fix(_, _, Conf, _, _), Conf >= 0.8),
            HighConf
        ),
        length(HighConf, HighConfCount),
        Stats = stats{
            total_fixes: FixCount,
            total_patterns: PatternCount,
            total_feedback: FeedbackCount,
            high_confidence_rules: HighConfCount
        }.

    %% Remove rules with low confidence (housekeeping)
    prune_low_confidence(Threshold) :-
        findall(
            learned_fix(A, B, C, D, E),
            (learned_fix(A, B, C, D, E), C < Threshold, D + E >= 5),
            ToRemove
        ),
        forall(member(Fact, ToRemove), retract(Fact)).

    %% Helper for timestamps (fallback if get_time not available)
    :- if(\+ current_predicate(get_time/1)).
    get_time(0).
    :- endif.

    exists_file(Path) :-
        catch(open(Path, read, S), _, fail),
        close(S).

:- end_object.
