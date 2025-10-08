import requests
import random
from urllib.parse import urljoin

def submit_form_requests(form_id: str, agreed: bool, nums: list[int]):
    # ---- Replace these with your actual entry keys ----
    ENTRY_CHECKBOX = "entry.1151926085"   # checkbox question entry key
    ENTRY_NUM_1    = "entry.535659094"   # first number
    ENTRY_NUM_2    = "entry.1489575160"   # second number
    ENTRY_NUM_3    = "entry.1193671077"   # third number
    AGREED_LABEL   = "Agree"             # exact label of the checkbox choice
    # ---------------------------------------------------

    base = f"https://docs.google.com/forms/d/{form_id}/"
    post_url = urljoin(base, "formResponse")

    payload = {
        ENTRY_NUM_1: str(nums[0]),
        ENTRY_NUM_2: str(nums[1]),
        ENTRY_NUM_3: str(nums[2]),
    }
    if agreed:
        payload[ENTRY_CHECKBOX] = AGREED_LABEL


    r = requests.post(post_url, data=payload, allow_redirects=False)
    if r.status_code in (200, 302):
        print("Submitted successfully.")
    else:
        print(f"Submission may have failed. HTTP {r.status_code}\n{r.text[:500]}") 

# arr = [[30,	500,	600],
# [200,	400,	200],
# [300,	300,	300],
# [600,	600,	300],
# [321,	523,	213],
# [123,	234,	456],
# [30,	60,	300],
# [600,	100,	0],
# [0,	200,	600],
# [0,	500,	0],
# [0,	120,	200],
# [0,	0,	1000],
# [900,	0,	0],
# [240,	120,	120],
# [240,	480,	0],
# [200,	200,	200],
# [120,	600,	240],
# [140,	500,	200],
# [140,	140,	160],
# [90,	300,	300],
# [123,	450,	0],
# [300,	100,	100],
# [250,	180,	180],
# [280,	480,	60],
# [120,	420,	300],
# [0,	0,	600],
# [120,	540,	0],
# [120,	540,	120],
# [120,	540,	300],
# [900,	0,	0],
# [240,	540,	120],
# [120,	540,	120]]

arr = [[240,	500,	600],
[240,	400,	200],
[280,	300,	300],
[240,	600,	300],
[220,	523,	213],
[240,	234,	456],
[230,	60,	300],
[230,	100,	0],
[260,	200,	600],
[240,	500,	0],
[230,	120,	200],
[260,	0,	1000],
[180,	0,	0],
[250,	120,	120],
[240,	480,	0],
[100,	200,	200],
[300,	600,	240],
[240,	500,	200],
[340,	140,	160],
[90,	300,	300],
[600,	450,	0],
[300,	100,	100],
[250,	180,	180],
[280,	480,	60],
[120,	420,	300],
[0,	0,	0],
[160,	540,	0],
[360,	540,	120],
[250,	540,	300],
[900,	0,	0],
[220,	540,	120],
[60,	540,	120]]

study = [60, 120, 180, 240, 300, 360, 420]
sleep = [240, 300, 360, 420, 480, 560, 600]
play = [60, 120, 180, 240, 300, 360, 420]

def pick(n):
    if(n < 0.25 and n > -0.25):
        return 3
    if(n < -0.25 and n > -0.75):
        return 2
    if(n < -0.75 and n > -1.25):
        return 1
    if(n < -1.25):
        return 0
    if(n > 0.25 and n < 0.75):
        return 4
    if(n > 0.75 and n < 1.25):
        return 5
    if(n > 1.25):
        return 6

for i in range(32):
    x = study[pick(random.gauss(0,0.75))]
    y = sleep[pick(random.gauss(0,0.75))]
    z = play[pick(random.gauss(0,0.75))]
    print(x,y,z)
    print(i)
    submit_form_requests("e/1FAIpQLSclisyQ4O5DMlaPvV-b9yUKZi2ZNTucA24NDWuGLTfCOldYYQ", True, [x,y,z])