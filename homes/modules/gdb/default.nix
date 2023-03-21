{ myLib, ... }: {
  xdg.configFile = myLib.copyTrees ./. [ "gdb" ] ;
}
