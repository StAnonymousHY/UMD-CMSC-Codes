import os, glob, csv, subprocess, sys, statistics

HERE = os.path.dirname(os.path.abspath(__file__))
PY = sys.executable
MATS_DIR = r"C:\Users\YHXHa\Desktop\UMD-CMSC-Codes\CMSC421_Project\Project_01\test"  # <-- change
SIZES = [10,15,20]

def run_lines(cmd, timeout_sec=120):
    p = subprocess.run(cmd, cwd=HERE, text=True, capture_output=True, timeout=timeout_sec)
    if p.returncode != 0:
        raise RuntimeError(p.stderr)
    return p.stdout.strip().splitlines()

def median(xs): return statistics.median(xs)

# Sweeps (minimal)
HC_RESTARTS = [10,50,200,800]
SA_LIMITS = [10000, 50000, 200000]         # keep k=2000, alpha=0.0005
GA_MUTS = [0.01,0.05,0.1,0.2]              # keep pop=100, gens=500

rows = [["algo","param","value","n","median_cost","median_real_ns","median_cpu_ns"]]

for n in SIZES:
    files = sorted(glob.glob(os.path.join(MATS_DIR, f"{n}_*.txt")))[:10]

    # HC sweep
    for r in HC_RESTARTS:
        block = []
        for f in files:
            out = run_lines([PY,"PartIIIHC.py",f,str(r)])
            print("HC:", out)
            block.append((float(out[1]), int(out[2]), int(out[3])))
        rows.append(["hc","restarts",r,n, median([b[0] for b in block]), int(median([b[1] for b in block])), int(median([b[2] for b in block]))])

    # SA sweep
    for L in SA_LIMITS:
        block = []
        for f in files:
            out = run_lines([PY,"PartIIISA.py",f,"2000","0.0005",str(L)])
            print("SA:", out)
            block.append((float(out[1]), int(out[2]), int(out[3])))
        rows.append(["sa","limit",L,n, median([b[0] for b in block]), int(median([b[1] for b in block])), int(median([b[2] for b in block]))])

    # GA sweep
    for m in GA_MUTS:
        block = []
        for f in files:
            out = run_lines([PY,"PartIIIGA.py",f,str(m),"100","500"])
            print("GA", out)
            block.append((float(out[1]), int(out[2]), int(out[3])))
        rows.append(["ga","mutation",m,n, median([b[0] for b in block]), int(median([b[1] for b in block])), int(median([b[2] for b in block]))])

with open("partIII_sweeps_medians.csv","w",newline="") as fp:
    csv.writer(fp).writerows(rows)

print("Wrote partIII_sweeps_medians.csv")