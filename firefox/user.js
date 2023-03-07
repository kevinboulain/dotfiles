// Scroll speed is way too fast under Wayland and setting it to 2 makes it
// behave like other applications.
// https://bugzilla.mozilla.org/show_bug.cgi?id=1752862
user_pref("apz.gtk.kinetic_scroll.delta_mode", 2);

// https://shivering-isles.com/til/2021/07/firefox-webrtc-popup
user_pref("privacy.webrtc.legacyGlobalIndicator", false);

// Never ask me to remember passwords.
user_pref("signon.rememberSignons", false);
