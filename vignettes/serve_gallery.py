from http.server import HTTPServer, SimpleHTTPRequestHandler
import os

os.chdir(os.path.dirname(os.path.abspath(__file__)))

class Handler(SimpleHTTPRequestHandler):
    def do_GET(self):
        self.path = "/gallery.html"
        return super().do_GET()

HTTPServer(("0.0.0.0", 8181), Handler).serve_forever()
