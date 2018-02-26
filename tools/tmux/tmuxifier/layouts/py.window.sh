# Set window root path. Default is `$session_root`.
# Must be called before `new_window`.
window_root "$(pwd)"

# Create new window. If no argument is given, window name will be based on
# layout file name.
new_window "$(pwd | sed 's@.*/@@')"

# |--------- ---------|
# |         |         |
# |         |    2    |
# |    1    |---------|
# |         |         |
# |         |    3    |
# |         |---------|
# |         |    4    |
# |         |---------|
# |         |    5    |
# |--------- ---------|

# Split window into panes.
split_h
# select_pane 1
# split_v 8
select_pane 2
split_v
split_v
split_v

# Run commands.
run_cmd "vim" 1
# run_cmd "make ghcid && echo \"ghcid ran from make\" || ghcid --height=8 --command=\"stack ghci --test\"" 2
run_cmd "git status"  2
run_cmd "ipython"     3
run_cmd "sos -p '([^_].*\.py)' -c 'python \1'" 4
run_cmd "sos -p '([^_].*\.py)' -c 'ctags -R --extra=+f --python-kinds=-i'"         5

# Set active pane.
select_pane 1

