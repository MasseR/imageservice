
{ pkgs, ... }:

{
  time.timeZone = "Europe/Helsinki";
  services.grafana = {
    enable = true;
    port = 3000;
    addr = "0.0.0.0";
    domain = "localhost";
    protocol = "http";
    dataDir = "/var/lib/grafana";
  };
  services.graphite = {
    carbon = {
      enableCache = true;
    };
    api = {
      enable = true;
      port = 8080;
    };
  };
  networking.firewall.allowedTCPPorts = [ 3000 8080 2003 ];
}
