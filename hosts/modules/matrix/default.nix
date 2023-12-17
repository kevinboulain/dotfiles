# TODO: https://matrix-org.github.io/synapse/latest/turn-howto.html
# https://nixos.org/manual/nixos/stable/index.html#module-services-matrix
{ config, lib, mySystemDirectory, pkgs, ... }:
let
  inherit (import ../nginx/lib.nix { inherit lib; }) mergeVirtualHostFragments virtualHostFragments;
  inherit (import ./lib.nix { inherit config; }) configuration;
  maxUploadSize = "50M";  # The default.
in
{
  services.nginx.virtualHosts.matrix = mergeVirtualHostFragments [
    virtualHostFragments.disallowRobots
    virtualHostFragments.explicitServerName
    {
      enableACME = true;
      forceSSL = true;
      # https://matrix-org.github.io/synapse/latest/reverse_proxy.html
      locations."~ ^/(_matrix|_synapse/client)/".extraConfig = ''
        proxy_pass http://localhost:8008;
        proxy_http_version 1.1;
        proxy_set_header X-Forwarded-For $remote_addr;
        proxy_set_header X-Forwarded-Proto $scheme;
        proxy_set_header Host $host;
        client_max_body_size ${maxUploadSize};
     '';
    }
  ];

  # https://github.com/vector-im/element-web/tree/develop#important-security-notes
  services.nginx.virtualHosts.element = mergeVirtualHostFragments [
    virtualHostFragments.disallowRobots
    virtualHostFragments.explicitServerName
    {
      enableACME = true;
      forceSSL = true;
      root = pkgs.element-web.override {
        conf = {
          default_server_config = configuration;
          default_theme = "dark";
          show_labs_settings = true;
        };
      };
      extraConfig = ''
        index index.html;
      '';
      locations."/".extraConfig = ''
        add_header Cache-Control "no-cache";
        add_header Content-Security-Policy "frame-ancestors 'self'";
        add_header X-Content-Type-Options nosniff;
        add_header X-Frame-Options SAMEORIGIN;
        add_header X-XSS-Protection "1; mode=block";
      '';
    }
  ];

  sops.secrets.matrixRegistrationSharedSecret = {
    key = "registration_shared_secret";
    group = config.systemd.services.matrix-synapse.serviceConfig.Group;
    mode = "0440";
  };

  services.matrix-synapse = {
    enable = true;
    dataDir = "${mySystemDirectory}/var/lib/matrix-synapse";
    # https://matrix-org.github.io/synapse/latest/usage/configuration/config_documentation.html
    settings = {
      listeners = [
        {
          bind_addresses = [ "::1" "127.0.0.1" ];
          port = 8008;
          type = "http";
          tls = false;
          x_forwarded = true;
          resources = [{
            names = [ "client" "federation" ];
            compress = true;
          }];
        }
        {
          bind_addresses = [ "::1" "127.0.0.1" ];
          port = 8018;
          type = "metrics";
          tls = false;
          resources = [];  # Required by the module.
        }
      ];
      enable_metrics = true;

      max_upload_size = maxUploadSize;
      # Might be a security issue, disabled for now.
      url_preview_enabled = false;
      # Not recommended for production, we'll see.
      # https://matrix-org.github.io/synapse/latest/setup/installation.html#using-postgresql
      database.name = "sqlite3";

      server_name = configuration."m.homeserver".server_name;
      public_baseurl = configuration."m.homeserver".base_url;

      # Should stay a private instance.
      enable_registration = false;
      registration_shared_secret_path = config.sops.secrets.matrixRegistrationSharedSecret.path;
      allow_guest_access = false;

      # Privacy.
      allow_public_rooms_over_federation = false;
      allow_public_rooms_without_auth = false;
      encryption_enabled_by_default_for_room_type = "all";
      include_profile_data_on_invite = false;
      limit_profile_requests_to_users_who_share_rooms = true;
      presence.enabled = false;
      report_stats = false;
      require_auth_for_profile_requests = true;
    };
  };

  # https://matrix-org.github.io/synapse/latest/metrics-howto.html
  services.prometheus.scrapeConfigs = [{
    job_name = "matrix-synapse";
    static_configs = [ { targets = [ "localhost:8018" ]; } ];
    metrics_path = "/_synapse/metrics";
  }];
}
