(require 's)

(defvar my-export-theme--faces
  `(("default:background" . ,(face-attribute 'default :background nil t))
    ("default:foreground" . ,(face-attribute 'default :foreground nil t))
    ("hl-line:background" . ,(face-attribute 'hl-line :background nil t))
    ("link-visited:foreground" . ,(face-attribute 'link-visited :foreground nil t))
    ("mode-line-buffer-id:foreground" . ,(face-attribute 'mode-line-buffer-id :foreground nil t))
    ("mode-line-buffer-id:weight" . ,(symbol-name (face-attribute 'mode-line-buffer-id :weight nil t)))
    ("mode-line:background" . ,(face-attribute 'mode-line :background nil t))
    ("mode-line:foreground" . ,(face-attribute 'mode-line :foreground nil t))))

(defun my-export-theme-foot ()
  (s-format "# (insert (my-export-theme-foot))
font = Iosevka Term:8

[colors]
background = ${default:background}
foreground = ${default:foreground}
urls = ${link-visited:foreground}
scrollback-indicator = ${mode-line:background} ${mode-line:foreground}
" 'aget (mapcar (lambda (face) `(,(car face) . ,(string-trim-left (cdr face) "#"))) my-export-theme--faces)))

(defun my-export-theme-sway ()
  (s-format "# (insert (my-export-theme-sway))
default_border pixel
smart_gaps on
gaps inner 3
output * bg ${default:background} solid_color
client.focused #00000000 ${mode-line:foreground} #00000000 ${mode-line:foreground}
client.focused_inactive #00000000 ${hl-line:background} #00000000 ${hl-line:background}
client.unfocused #00000000 ${hl-line:background} #00000000 ${hl-line:background}
" 'aget my-export-theme--faces))

(defun my-export-theme-mako ()
  (s-format "# (insert (my-export-theme-mako))
font=Iosevka Term 8
background-color=${default:background}
text-color=${default:foreground}
border-color=${mode-line:foreground}
progress-color=over ${hl-line:background}
" 'aget my-export-theme--faces))

(defun my-export-theme-tmux ()
  (s-format "# (insert (my-export-theme-tmux))
setw -g window-style \"fg=${default:foreground},bg=${default:background}\"
setw -g pane-active-border-style \"fg=${hl-line:background},bg=${default:background}\"
setw -g pane-border-style \"fg=${hl-line:background},bg=${default:background}\"

setw -g status-fg \"${mode-line:foreground}\"
setw -g status-bg \"${mode-line:background}\"
setw -g message-style \"fg=${mode-line:foreground},bg=${mode-line:background}\"

setw -g window-status-style \"fg=${link-visited:foreground}\"
setw -g window-status-current-style \"fg=${mode-line-buffer-id:foreground},${mode-line-buffer-id:weight}\"
setw -g mode-style \"bg=${hl-line:background}\"

setw -g display-panes-colour \"${link-visited:foreground}\"
setw -g display-panes-active-colour \"${mode-line-buffer-id:foreground}\"
" 'aget my-export-theme--faces))

(provide 'my-export-theme)
