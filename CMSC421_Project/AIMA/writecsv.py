import os, glob, csv, subprocess, sys

HERE = os.path.dirname(os.path.abspath(__file__))          # cmsc421/AIMA
PY = sys.executable

MATS_DIR = os.path.abspath(os.path.join(HERE, "..", "Project_01", "test"))

def run_lines(cmd):
    out = subprocess.check_output(cmd, cwd=HERE, text=True).strip().splitlines()
    return out

def partI(out_csv):
    algos = [
        ("nn",   lambda m: [PY, "PartI.py", m, "nn"]),
        ("nn2",  lambda m: [PY, "PartI.py", m, "nn2"]),
        ("rrnn", lambda m: [PY, "PartI.py", m, "rrnn", "2", "800"]),
    ]
    rows = [["matrix","algo","cost","real_ns","cpu_ns"]]
    for f in sorted(glob.glob(os.path.join(MATS_DIR, "*.txt"))):
        for name, mk in algos:
            print("running:", os.path.basename(f), name)
            lines = run_lines(mk(f))
            # expected: tour, cost, real_ns, cpu_ns
            rows.append([os.path.basename(f), name, float(lines[1]), int(lines[2]), int(lines[3])])
    with open(out_csv, "w", newline="") as fp:
        csv.writer(fp).writerows(rows)

def partII(out_csv):
    rows = [["matrix","cost","real_ns","cpu_ns","expanded"]]
    for f in sorted(glob.glob(os.path.join(MATS_DIR, "*.txt"))):
        print("running:", f)
        lines = run_lines([PY, "PartII.py", f])
        # expected: tour, cost, real_ns, cpu_ns, expanded
        rows.append([os.path.basename(f), float(lines[1]), int(lines[2]), int(lines[3]), int(lines[4])])
    with open(out_csv, "w", newline="") as fp:
        csv.writer(fp).writerows(rows)

def partIII(out_csv):
    algos = [
        ("hc", lambda m: [PY, "PartIIIHC.py", m, "200"]),
        ("sa", lambda m: [PY, "PartIIISA.py", m, "2000", "0.0005", "50000"]),
        ("ga", lambda m: [PY, "PartIIIGA.py", m, "0.1", "100", "500"]),
    ]
    rows = [["matrix","algo","cost","real_ns","cpu_ns"]]
    for f in sorted(glob.glob(os.path.join(MATS_DIR, "*.txt"))):
        for name, mk in algos:
            print("running:", os.path.basename(f), name)
            lines = run_lines(mk(f))
            # expected: tour, cost, real_ns, cpu_ns
            rows.append([os.path.basename(f), name, float(lines[1]), int(lines[2]), int(lines[3])])
    with open(out_csv, "w", newline="") as fp:
        csv.writer(fp).writerows(rows)

if __name__ == "__main__":
    print("Matrices:", MATS_DIR)

    # partI(os.path.join(HERE, "partI_allruns.csv"))
    partII(os.path.join(HERE, "partII_allruns.csv"))
    # partIII(os.path.join(HERE, "partIII_allruns.csv"))

    print("Wrote CSVs into:", HERE)