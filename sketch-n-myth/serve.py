from http.server import *
import subprocess
import time

PORT = 9090
TIMEOUT = None
COOLDOWN = None

EVAL_TIMED_OUT = \
    '["Error", "Evaluation timed out."]'

SYNTHESIS_TIMED_OUT = \
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
        return self.rfile.read(content_len)

    # OPTIONS is needed for CORS preflight
    def do_OPTIONS(self):
        self.respond_ok()

    # POST is the main REST API
    def do_POST(self):
        command = self.path[1:]
        user_input = self.get_body()
        try:
            output = subprocess.check_output(
                ["./smyth", command],
                input=user_input,
                timeout=TIMEOUT
            )
            if COOLDOWN:
                time.sleep(COOLDOWN)
            self.respond_ok_bytes(output)
        except subprocess.CalledProcessError as cpe:
            if cpe.returncode == 2:
                self.respond_not_found()
            else:
                self.respond_server_error()
        except subprocess.TimeoutExpired:
            if command == "eval":
                self.respond_ok(EVAL_TIMED_OUT)
            elif command == "synthesize":
                self.respond_ok(SYNTHESIS_TIMED_OUT)
            else:
                self.respond_server_error()

def run(server_class=HTTPServer, handler_class=Handler):
    server_address = ('', PORT)
    httpd = server_class(server_address, handler_class)
    httpd.serve_forever()

if __name__ == "__main__":
    run()
