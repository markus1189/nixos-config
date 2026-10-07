# Local stand-in for OpenRouter and Requesty: replays scripted responses per URL path.
# usage: python3 stub.py SCRIPT.json PORTFILE
#   SCRIPT.json: {"/v1/systemone": [{"status": 200, "body": {...}, "headers": {...}}, ...], ...}
#   Each path's responses are served in order; the last one repeats. Requests are appended
#   to SCRIPT.json.log as JSON lines {path, auth, body}.
import json, sys
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

script = json.load(open(sys.argv[1]))
served = {p: 0 for p in script}


class H(BaseHTTPRequestHandler):
    def do_POST(self):
        body = self.rfile.read(int(self.headers.get("Content-Length", 0)))
        with open(sys.argv[1] + ".log", "a") as f:
            f.write(json.dumps({"path": self.path, "auth": self.headers.get("Authorization"),
                                "body": json.loads(body)}) + "\n")
        seq = script.get(self.path) or [{"status": 404, "body": {"error": {"message": "no script"}}}]
        r = seq[min(served.get(self.path, 0), len(seq) - 1)]
        served[self.path] = served.get(self.path, 0) + 1
        if r.get("drop"):
            self.close_connection = True
            return  # no response at all: the client sees a dropped connection
        out = r["body"] if isinstance(r["body"], str) else json.dumps(r["body"])
        self.send_response(r.get("status", 200))
        for k, v in r.get("headers", {}).items():
            self.send_header(k, v)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(out.encode())))
        self.end_headers()
        self.wfile.write(out.encode())

    def log_message(self, *a):
        pass


srv = ThreadingHTTPServer(("127.0.0.1", 0), H)
open(sys.argv[2], "w").write(str(srv.server_address[1]))
srv.serve_forever()
