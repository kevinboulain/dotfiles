{ ... }:
{
  sops.secrets.matrixRegistrationSharedSecret.sopsFile = ./matrix.yaml;
  sops.secrets.matrixTurnSharedSecret.sopsFile = ./matrix.yaml;
}
