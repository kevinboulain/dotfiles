{ myStateDirectory, ... }: {
  sopsUserPassword = {
    sopsFile = ./lib.yaml;
    neededForUsers = true;
  };
  userHomeDirectory = "${myStateDirectory}/users";
}
