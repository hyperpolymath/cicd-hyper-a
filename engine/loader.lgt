%% SPDX-License-Identifier: AGPL-3.0-or-later
%% cicd-hyper-a Engine Loader

:- initialization((
    logtalk_load([
        rules(cicd_rules),
        rules(rule_distiller),
        rules(forge_adapters),
        rules(learning)
    ], []),
    write('cicd-hyper-a rule engine loaded.\n'),
    write('Available objects: cicd_rules, rule_distiller, forge_adapters, learning\n'),

    % Auto-load saved knowledge if available
    ( catch(learning::load_knowledge('knowledge.pl'), _, true) ->
        write('Loaded saved knowledge base.\n')
    ; write('Starting with fresh knowledge base.\n')
    ),

    % Bootstrap from training data if learning is empty
    ( learning::get_learning_stats(Stats),
      Stats.total_patterns =:= 0 ->
        write('Bootstrapping from training data...\n'),
        ( catch(rule_distiller::load_training_data('../.audittraining/training-data.pl'), _, fail) ->
            rule_distiller::distill_rules,
            write('Training data loaded and rules distilled.\n')
        ; write('No training data found, starting empty.\n')
        )
    ; true
    )
)).
