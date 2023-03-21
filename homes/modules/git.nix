{ pkgs, ... }: {
  programs.git = {
    enable = true;
    # I could (gitMinimal.override { sendEmailSupport = true; }) to do without
    # the UI but that wouldn't be cached.
    package = pkgs.gitFull;
    # So I can locally override some settings, like user.email & user.name.
    includes = [{ path = "~/.config/git/local"; }];
    extraConfig = {
      alias = {
        ft = "fetch --all --tags --prune";
        # git fp outgoing -1
        # git send-email outgoing/* --to $email
        fp = "format-patch --find-renames --find-copies --cover-letter --no-signature --output-directory";
        # %C(auto) may not work on older git version
        # lg = log --graph --date=short --pretty=tformat:'%h -%d %s (%an, %ad)'
        lg = "log --graph --date=short --pretty=tformat:'%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%an, %ad)%Creset'";
        st = "status";
      };
      core.whitespace = "space-before-tab, tab-in-indent, trailing-space";
      # Avoid storing credentials (Homebrew's git config --system
      # credential.helper is set to osxkeychain).
      credential.helper = "";
      diff = {
        algorithm = "patience";
        mnemonicPrefix = "true";
      };
      format.pretty = "fuller";
      # Will influence pull.ff if unspecified.
      merge.ff = "only";
      # Always specify refspec.
      push.default = "nothing";
      rebase.missingCommitsCheck = "warn";
      grep.patternType = "perl";
    };
  };
}
