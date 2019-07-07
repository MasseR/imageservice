
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
      config = ''
         [cache]
         UDP_RECEIVER_INTERFACE = 0.0.0.0
         PICKLE_RECEIVER_INTERFACE = 0.0.0.0
         LINE_RECEIVER_INTERFACE = 0.0.0.0
         CACHE_QUERY_INTERFACE = 0.0.0.0
         # Do not log every update
         LOG_UPDATES = False
         LOG_CACHE_HITS = False
      '';
    };
    api = {
      enable = true;
      port = 8080;
    };
  };
  networking.firewall.allowedTCPPorts = [ 3000 8080 2003 ];
}
