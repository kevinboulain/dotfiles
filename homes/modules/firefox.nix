{ pkgs, ... }: {
  programs.firefox = {
    enable = true;
    package = pkgs.firefox-wayland;
    profiles."default".settings = {
      # Scroll speed is way too fast under Wayland and setting it to 2 makes
      # it behave like other applications.
      # https://bugzilla.mozilla.org/show_bug.cgi?id=1752862
      "apz.gtk.kinetic_scroll.delta_mode" =  2;
      # https://shivering-isles.com/til/2021/07/firefox-webrtc-popup
      "privacy.webrtc.legacyGlobalIndicator" = false;
      # Never ask me to remember passwords.
      "signon.rememberSignons" = false;
      # Kinda best-effort (ignores the GTK theme)...
      "devtools.editor.keymap" = "emacs";
      # Always start in private browsing.
      "browser.privatebrowsing.autostart" = true;
      # Why isn't that the default...
      "privacy.resistFingerprinting" = true;
    };
  };
}
