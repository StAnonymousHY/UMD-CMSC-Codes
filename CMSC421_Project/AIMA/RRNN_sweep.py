import os, glob, csv, subprocess, sys, re, statistics

HERE = os.path.dirname(os.path.abspath(__file__))
PY = sys.executable

MATS_DIR = r"C:\Users\YHXHa\Desktop\UMD-CMSC-Codes\CMSC421_Project\Project_01\test"  # <-- change if needed
SIZES = [10,15,20]  # adjust to what you have
KS = [2,3,4,5]
RESTARTS = [10, 100, 200, 800]   # keep small so it finishes

def run_lines(cmd, timeout_sec=120):
    p = subprocess.run(cmd, cwd=HERE, text=True, capture_output=True, timeout=timeout_sec)
    if p.returncode != 0:
        raise RuntimeError(p.stderr)
    return p.stdout.strip().splitlines()

def median(xs): return statistics.median(xs)

allruns = [["n","matrix","k","restarts","cost","real_ns","cpu_ns"]]

for n in SIZES:
    files = sorted(glob.glob(os.path.join(MATS_DIR, f"{n}_*.txt")))[:10]
    for f in files:
        for k in KS:
            for r in RESTARTS:
                cmd = [PY, "PartI.py", f, "rrnn", str(k), str(r)]
                print(cmd)
                try:
                    out = run_lines(cmd)
                except subprocess.TimeoutExpired:
                    print("TIMEOUT:", os.path.basename(f), "k", k, "r", r)
                    continue
                allruns.append([n, os.path.basename(f), k, r, float(out[1]), int(out[2]), int(out[3])])
                print("done", n, os.path.basename(f), "k", k, "r", r)

with open("rrnn_sweep_allruns.csv","w",newline="") as fp:
    csv.writer(fp).writerows(allruns)

# medians
rows = [["n","k","restarts","median_cost","median_real_ns","median_cpu_ns"]]
data = allruns[1:]
for n in SIZES:
    for k in KS:
        for r in RESTARTS:
            block = [x for x in data if x[0]==n and x[2]==k and x[3]==r]
            if not block: 
                continue
            rows.append([n,k,r,
                         median([b[4] for b in block]),
                         int(median([b[5] for b in block])),
                         int(median([b[6] for b in block]))])

with open("rrnn_sweep_medians.csv","w",newline="") as fp:
    csv.writer(fp).writerows(rows)

print("Wrote rrnn_sweep_allruns.csv and rrnn_sweep_medians.csv")