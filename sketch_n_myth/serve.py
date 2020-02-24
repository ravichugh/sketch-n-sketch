from http.server import *
import subprocess

TIMEOUT = 5

TIMED_OUT_STRING = \
    '["Ok",{"time_taken":' \
        + str(TIMEOUT) \
        + ',"hole_fillings":[],"timed_out":true}]'

class Handler(BaseHTTPRequestHandler):
    def send_cors_headers(self):
        self.send_header("Access-Control-Allow-Origin", "*")
        self.send_header("Access-Control-Allow-Headers", "Accept, Content-Type")
        self.send_header("Access-Control-Allow-Methods", "GET, POST, OPTIONS")

    def respond_ok_bytes(self, data_bytes=None):
        self.send_response(200)
        self.send_cors_headers()
        self.end_headers()
        if data_bytes:
            self.wfile.write(data_bytes)

    def respond_ok(self, data=None):
        self.respond_ok_bytes(bytes(data, "utf8") if data else None)

    def respond_not_found(self):
        self.send_response(404)
        self.send_cors_headers()
        self.end_headers()

    def respond_server_error(self):
        self.send_response(500)
        self.send_cors_headers()
        self.end_headers()

    def get_body(self):
        content_len = int(self.headers.get('Content-Length'))
        return self.rfile.read(content_len).decode("utf8")

    def do_OPTIONS(self):
        self.respond_ok()

    def do_POST(self):
        command = self.path[1:]
        arg = self.get_body()
        try:
            output = subprocess.check_output(
                ["./snm", command, arg],
                timeout=TIMEOUT
            )
            self.respond_ok_bytes(output)
        except subprocess.CalledProcessError as cpe:
            if cpe.returncode == 2:
                self.respond_not_found()
            else:
                self.respond_server_error()
        except subprocess.TimeoutExpired:
            self.respond_ok(TIMED_OUT_STRING)

def run(server_class=HTTPServer, handler_class=Handler):
    server_address = ('', 9090)
    httpd = server_class(server_address, handler_class)
    httpd.serve_forever()

if __name__ == "__main__":
    run()
