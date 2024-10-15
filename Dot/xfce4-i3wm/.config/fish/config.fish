# After all exports in .zshrc, add:
# if [[ $- = *i* ]]; then
#   exec fish
# fi

# Functions _puffer* copied from nickeb96/puffer-fish
function _puffer_fish_expand_lastarg
    switch (commandline -t)
      case '!'
        commandline -t ""
        commandline -f history-token-search-backward
      case '*'
        commandline -i '$'
    end
end

function _puffer_fish_expand_bang
    switch (commandline -t)
      case '!'
        commandline -t $history[1]
      case '*'
        commandline -i '!'
    end
end

if status is-interactive
    function fish_mode_prompt; end # Disable the default mode indicator to change its position in the prompt

    bind --mode insert \ck up-or-search
    bind --mode insert \cj down-or-search
    bind --mode default h prevd-or-backward-word
    bind --mode default l nextd-or-forward-word

    bind --mode insert ! _puffer_fish_expand_bang
    bind --mode insert '$' _puffer_fish_expand_lastarg
    bind --mode default --erase ! '$'

    source /usr/share/doc/fzf/examples/key-bindings.fish
    fzf_key_bindings

    if test vterm != "$INSIDE_EMACS"
        set -x fish_cursor_default block
        set -x fish_cursor_insert line
        set -x fish_cursor_replace_one underscore
        set -x fish_cursor_replace underscore
        set -x fish_cursor_external line
        set -x fish_cursor_visual block
        fish_vi_key_bindings
    end

    abbr --add ls eza
    abbr --add l eza -lag
    abbr --add 2up "sudo apt update && sudo apt upgrade"
    abbr --add !\$ --position anywhere --function history_last_arg

    set -x fish_greeting # Disable greeting
    set -x fish_autosuggestion_enabled 0

    set -x fish_color_normal normal
    set -x fish_color_command -d --bold green
    set -x fish_color_param brblue
    set -x fish_color_quote green
    set -x fish_color_redirection --bold blue
    set -x fish_color_end brblue
    set -x fish_color_error --bold brred
    set -x fish_color_comment brgreen
    set -x fish_color_selection -d -r magenta
    set -x fish_color_search_match green
    set -x fish_color_operator --bold yellow
    set -x fish_color_escape cyan
    set -x fish_color_cwd -d --bold brblue
    set -x fish_color_user brcyan
    set -x fish_color_host cyan
    set -x fish_color_host_remote --bold red
    set -x fish_color_cancel red
end
