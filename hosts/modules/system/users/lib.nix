{ myStateDirectory, ... }: {
  sopsUserPassword = sopsFile: {
    inherit sopsFile;
    neededForUsers = true;
  };
  userHomeDirectory = "${myStateDirectory}/users";
}
