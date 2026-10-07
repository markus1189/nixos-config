# Drives jev.call() against scripted fake HTTP responses; no network, no sleeping.
# run(responses, backend="openrouter", questions=None) -> {"result", "stop", "calls", "sleeps"}
# A response is {"status": int, "body": obj|str, "headers": {...}} or {"raise": "reset"|"disconnect"}.
import http.client, io, json, os, sys, threading, urllib.error

sys.path.insert(0, os.environ["JEV_DIR"])
import jev  # noqa: E402

Q = {"u": {"type": "noul", "instructions": "x"}}
URL = "https://stub.test/v1/x"


class Resp(io.BytesIO):
    def __enter__(self): return self
    def __exit__(self, *a): pass


def run(responses, backend="openrouter", questions=None):
    calls, sleeps, seq = [], [], list(responses)

    def urlopen(req, timeout=0):
        calls.append(req.full_url)
        r = seq.pop(0) if len(seq) > 1 else seq[0]
        if r.get("raise") == "reset": raise ConnectionResetError("reset by peer")
        if r.get("raise") == "disconnect": raise http.client.RemoteDisconnected("closed")
        body = r["body"] if isinstance(r["body"], str) else json.dumps(r["body"])
        if r.get("status", 200) >= 400:
            raise urllib.error.HTTPError(URL, r["status"], "err", r.get("headers", {}), io.BytesIO(body.encode()))
        return Resp(body.encode())

    def sleep(sec):
        if not sec >= 0:  # like time.sleep: negative or NaN raises
            raise ValueError(f"sleep length {sec!r}")
        sleeps.append(sec)

    jev.urllib.request.urlopen = urlopen
    jev.time.sleep = sleep
    stop = threading.Event()
    result = jev.call(backend, URL, {}, questions or Q, "k", stop)
    return {"result": result, "stop": stop.is_set(), "calls": len(calls), "sleeps": sleeps}


if __name__ == "__main__":
    print(json.dumps(run(*json.loads(sys.argv[1]))))
