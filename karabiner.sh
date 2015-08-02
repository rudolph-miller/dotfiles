#!/bin/sh

cli=/Applications/Karabiner.app/Contents/Library/bin/karabiner

$cli set repeat.initial_wait 100
/bin/echo -n .
$cli set option.vimode_commandl_hjkl 1
/bin/echo -n .
$cli set remap.jis_command2eisuukana_prefer_command 1
/bin/echo -n .
$cli set remap.jis_yen2backslash 1
/bin/echo -n .
$cli set repeat.wait 20
/bin/echo -n .
$cli set remap.jis_eisuu2controlL_eisuu 1
/bin/echo -n .
$cli set remap.jis_jansi 1
/bin/echo -n .
$cli set remap.swap_number_and_symbol 1
/bin/echo -n .
$cli set remap.jis_kana2controlL_kana 1
/bin/echo -n .
$cli set remap.qwerty2dvorak 1
/bin/echo -n .
$cli set remap.delete2forwarddelete_fnDelete2delete 1
/bin/echo -n .
/bin/echo
