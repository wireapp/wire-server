class Response:
    def __init__(self, method, url, request, response):
        self.method = method
        self.url = url
        self.request = request
        self.response = response

    def __enter__(self):
        return self.response

    def __exit__(self, etype, evalue, traceback):
        if evalue is not None:
            print(f"{self.method} {self.url}:")

            # print request JSON if present
            req = self.request.get('json')
            if req is not None:
                print("request body:")
                print(json.dumps(req, indent=True))

            # print response status code and JSON if present
            print("status code:", self.response.status_code)
            try:
                resp = self.response.json()
                print("response body:")
                print(json.dumps(resp, indent=True))
            except requests.exceptions.JSONDecodeError:
                pass
