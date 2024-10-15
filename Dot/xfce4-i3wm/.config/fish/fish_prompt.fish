function fish_prompt
    set --local ssh_part
    if set -q SSH_CLIENT
       set ssh_part (prompt_login)
    end
    set --local pwd_part (set_color $fish_color_cwd) (prompt_pwd) (set_color normal)
    set --local mode_part
    switch $fish_bind_mode
        case default
            set mode_part (set_color --bold red) '<' (set_color normal)
        case insert
            set mode_part (set_color --bold green) '>' (set_color normal)
        case replace_one
            set mode_part (set_color --bold cyan) '-' (set_color normal)
        case visual
            set mode_part (set_color --bold magenta) '/' (set_color normal)
    end
    echo -n -s $ssh_part $pwd_part (fish_git_prompt) ' ' $mode_part ' '
end

function fish_right_prompt
    set --local last_status $status
    set --local stat_part
    if test $last_status -ne 0
        set stat_part (set_color red)" [$last_status]"(set_color normal)
    end

    # Copied from jorgebucaran/humantime.fish
    set --local secs (math --scale=1 $CMD_DURATION/1000 % 60)
    set --local mins (math --scale=0 $CMD_DURATION/60000 % 60)
    set --local hours (math --scale=0 $CMD_DURATION/3600000)
    test $hours -gt 0 && set --local --append out $hours"h"
    test $mins -gt 0 && set --local --append out $mins"m"
    test $secs -gt 0 && set --local --append out $secs"s"

    printf "%s%s" $out $stat_part
end
