{ port = +8000
, dbPath = "/tmp/imageservice"
, services = {
    imgur = "asd"
  }
, carbon = {
    host = "localhost"
  , port = +2003
  }
, workers = [{ subreddits = ["wallpapers", "pics"] }] }
