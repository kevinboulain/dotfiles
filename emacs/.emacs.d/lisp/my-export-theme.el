(require 's)

(defvar my-export-theme--faces
  `(("background" . ,(alist-get "color-4" (tao-theme-yin-palette) nil nil #'equal))
    ("light-highlight" . ,(alist-get "color-5" (tao-theme-yin-palette) nil nil #'equal))
    ("light-highlight-accent" . ,(alist-get "color-6" (tao-theme-yin-palette) nil nil #'equal))
    ("status" . ,(alist-get "color-7" (tao-theme-yin-palette) nil nil #'equal))
    ("status-accent" . ,(alist-get "color-8" (tao-theme-yin-palette) nil nil #'equal))
    ("foreground" . ,(alist-get "color-10" (tao-theme-yin-palette) nil nil #'equal))
    ("strong-highlight" . ,(alist-get "color-11" (tao-theme-yin-palette) nil nil #'equal))
    ("important" . ,(alist-get "color-14" (tao-theme-yin-palette) nil nil #'equal))))

(defun my-export-theme-foot ()
  (s-format "# (insert (my-export-theme-foot))
font = Iosevka Term:size=10

[colors]
background = ${background}
foreground = ${foreground}
urls = ${strong-highlight}
scrollback-indicator = ${background} ${status}
" 'aget (mapcar (lambda (face) `(,(car face) . ,(string-trim-left (cdr face) "#"))) my-export-theme--faces)))

(defun my-export-theme-sway ()
  (s-format "# (insert (my-export-theme-sway))
default_border pixel 1
smart_gaps on
gaps inner 3
output * bg ${background} solid_color
client.focused #00000000 ${status} #00000000 ${status-accent}
client.focused_inactive #00000000 ${light-highlight} #00000000 ${light-highlight-accent}
client.unfocused #00000000 ${light-highlight} #00000000 ${light-highlight-accent}

bar {
  mode dock
  position bottom
  font \"Iosevka Term 8\"
  status_command ~/.cargo/bin/swaybar 2>> /tmp/swaybar_$USER.log
  colors {
    background ${background}
    statusline ${foreground}
    separator ${status}
    focused_workspace ${background} ${background} ${important}
    active_workspace ${background} ${background} ${foreground}
    inactive_workspace ${background} ${background} ${status}
  }
}" 'aget my-export-theme--faces))

(defun my-export-theme-mako ()
  (s-format "# (insert (my-export-theme-mako))
font=Iosevka Term 10
background-color=${background}
text-color=${foreground}
border-color=${status}
progress-color=over ${light-highlight}
" 'aget my-export-theme--faces))

(defun my-export-theme-tmux ()
  (s-format "# (insert (my-export-theme-tmux))
setw -g window-style \"fg=${foreground},bg=${background}\"
setw -g pane-active-border-style \"fg=${light-highlight},bg=${background}\"
setw -g pane-border-style \"fg=${light-highlight},bg=${background}\"

setw -g status-style \"fg=${important},bg=${background}\"
setw -g message-style \"fg=${status},bg=${background}\"

setw -g window-status-style \"fg=${status}\"
setw -g window-status-current-style \"fg=${important},bold\"
setw -g mode-style \"bg=${light-highlight}\"

setw -g display-panes-colour \"${strong-highlight}\"
setw -g display-panes-active-colour \"${important}\"
" 'aget my-export-theme--faces))

(provide 'my-export-theme)
