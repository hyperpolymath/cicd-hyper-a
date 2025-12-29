%% SPDX-License-Identifier: AGPL-3.0-or-later
%% cicd-hyper-a Engine Loader

:- initialization((
    logtalk_load([
        rules(cicd_rules),
        rules(rule_distiller),
        rules(forge_adapters)
    ], []),
    write('cicd-hyper-a rule engine loaded.\n'),
    write('Available objects: cicd_rules, rule_distiller, forge_adapters\n')
)).
