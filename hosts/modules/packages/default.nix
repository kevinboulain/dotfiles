{ ... }: {
  # Get rid of any package installed by default (like nano).
  environment.defaultPackages = [];
}
