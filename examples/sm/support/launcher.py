#!/usr/bin/env python

import os
import math

ttest = [
    [0, 60.0, 66.7, 75.0, 80.0, 87.5, 90.0, 95.0, 97.5, 99.0, 99.5, 99.9],
    [1, 0.325, 0.577, 1.000, 1.376, 2.414, 3.078, 6.314, 12.706, 31.821, 63.657, 318.31],
    [2, 0.289, 0.500, 0.816, 1.061, 1.604, 1.886, 2.920, 4.303, 6.965, 9.925, 22.327],
    [3, 0.277, 0.476, 0.765, 0.978, 1.423, 1.638, 2.353, 3.182, 4.541, 5.841, 10.215],
    [4, 0.271, 0.464, 0.741, 0.941, 1.344, 1.533, 2.132, 2.776, 3.747, 4.604, 7.173],
    [5, 0.267, 0.457, 0.727, 0.920, 1.301, 1.476, 2.015, 2.571, 3.365, 4.032, 5.893],
    [6, 0.265, 0.453, 0.718, 0.906, 1.273, 1.440, 1.943, 2.447, 3.143, 3.707, 5.208],
    [7, 0.263, 0.449, 0.711, 0.896, 1.254, 1.415, 1.895, 2.365, 2.998, 3.499, 4.785],
    [8, 0.262, 0.447, 0.706, 0.889, 1.240, 1.397, 1.860, 2.306, 2.896, 3.355, 4.501],
    [9, 0.261, 0.445, 0.703, 0.883, 1.230, 1.383, 1.833, 2.262, 2.821, 3.250, 4.297],
    [10, 0.260, 0.444, 0.700, 0.879, 1.221, 1.372, 1.812, 2.228, 2.764, 3.169, 4.144],
    [11, 0.260, 0.443, 0.697, 0.876, 1.214, 1.363, 1.796, 2.201, 2.718, 3.106, 4.025],
    [12, 0.259, 0.442, 0.695, 0.873, 1.209, 1.356, 1.782, 2.179, 2.681, 3.055, 3.930],
    [13, 0.259, 0.441, 0.694, 0.870, 1.204, 1.350, 1.771, 2.160, 2.650, 3.012, 3.852],
    [14, 0.258, 0.440, 0.692, 0.868, 1.200, 1.345, 1.761, 2.145, 2.624, 2.977, 3.787],
    [15, 0.258, 0.439, 0.691, 0.866, 1.197, 1.341, 1.753, 2.131, 2.602, 2.947, 3.733],
    [16, 0.258, 0.439, 0.690, 0.865, 1.194, 1.337, 1.746, 2.120, 2.583, 2.921, 3.686],
    [17, 0.257, 0.438, 0.689, 0.863, 1.191, 1.333, 1.740, 2.110, 2.567, 2.898, 3.646],
    [18, 0.257, 0.438, 0.688, 0.862, 1.189, 1.330, 1.734, 2.101, 2.552, 2.878, 3.610],
    [19, 0.257, 0.438, 0.688, 0.861, 1.187, 1.328, 1.729, 2.093, 2.539, 2.861, 3.579],
    [20, 0.257, 0.437, 0.687, 0.860, 1.185, 1.325, 1.725, 2.086, 2.528, 2.845, 3.552],
    [21, 0.257, 0.437, 0.686, 0.859, 1.183, 1.323, 1.721, 2.080, 2.518, 2.831, 3.527],
    [22, 0.256, 0.437, 0.686, 0.858, 1.182, 1.321, 1.717, 2.074, 2.508, 2.819, 3.505],
    [23, 0.256, 0.436, 0.685, 0.858, 1.180, 1.319, 1.714, 2.069, 2.500, 2.807, 3.485],
    [24, 0.256, 0.436, 0.685, 0.857, 1.179, 1.318, 1.711, 2.064, 2.492, 2.797, 3.467],
    [25, 0.256, 0.436, 0.684, 0.856, 1.178, 1.316, 1.708, 2.060, 2.485, 2.787, 3.450],
    [26, 0.256, 0.436, 0.684, 0.856, 1.177, 1.315, 1.706, 2.056, 2.479, 2.779, 3.435],
    [27, 0.256, 0.435, 0.684, 0.855, 1.176, 1.314, 1.703, 2.052, 2.473, 2.771, 3.421],
    [28, 0.256, 0.435, 0.683, 0.855, 1.175, 1.313, 1.701, 2.048, 2.467, 2.763, 3.408],
    [29, 0.256, 0.435, 0.683, 0.854, 1.174, 1.311, 1.699, 2.045, 2.462, 2.756, 3.396],
    [30, 0.256, 0.435, 0.683, 0.854, 1.173, 1.310, 1.697, 2.042, 2.457, 2.750, 3.385],
    [31, 0.256, 0.435, 0.683, 0.854, 1.173, 1.310, 1.697, 2.042, 2.457, 2.750, 3.385],
    [32, 0.256, 0.435, 0.683, 0.854, 1.173, 1.310, 1.697, 2.042, 2.457, 2.750, 3.385],
    [33, 0.256, 0.435, 0.683, 0.854, 1.173, 1.310, 1.697, 2.042, 2.457, 2.750, 3.385],
    [34, 0.256, 0.435, 0.683, 0.854, 1.173, 1.310, 1.697, 2.042, 2.457, 2.750, 3.385],
    [35, 0.255, 0.434, 0.682, 0.852, 1.170, 1.306, 1.690, 2.030, 2.438, 2.724, 3.340],
    [36, 0.255, 0.434, 0.682, 0.852, 1.170, 1.306, 1.690, 2.030, 2.438, 2.724, 3.340],
    [37, 0.255, 0.434, 0.682, 0.852, 1.170, 1.306, 1.690, 2.030, 2.438, 2.724, 3.340],
    [38, 0.255, 0.434, 0.682, 0.852, 1.170, 1.306, 1.690, 2.030, 2.438, 2.724, 3.340],
    [39, 0.255, 0.434, 0.682, 0.852, 1.170, 1.306, 1.690, 2.030, 2.438, 2.724, 3.340],
    [40, 0.255, 0.434, 0.681, 0.851, 1.167, 1.303, 1.684, 2.021, 2.423, 2.704, 3.307],
    [41, 0.255, 0.434, 0.681, 0.851, 1.167, 1.303, 1.684, 2.021, 2.423, 2.704, 3.307],
    [42, 0.255, 0.434, 0.681, 0.851, 1.167, 1.303, 1.684, 2.021, 2.423, 2.704, 3.307],
    [43, 0.255, 0.434, 0.681, 0.851, 1.167, 1.303, 1.684, 2.021, 2.423, 2.704, 3.307],
    [44, 0.255, 0.434, 0.681, 0.851, 1.167, 1.303, 1.684, 2.021, 2.423, 2.704, 3.307],
    [45, 0.255, 0.434, 0.680, 0.850, 1.165, 1.301, 1.679, 2.014, 2.412, 2.690, 3.281],
    [46, 0.255, 0.434, 0.680, 0.850, 1.165, 1.301, 1.679, 2.014, 2.412, 2.690, 3.281],
    [47, 0.255, 0.434, 0.680, 0.850, 1.165, 1.301, 1.679, 2.014, 2.412, 2.690, 3.281],
    [48, 0.255, 0.434, 0.680, 0.850, 1.165, 1.301, 1.679, 2.014, 2.412, 2.690, 3.281],
    [49, 0.255, 0.434, 0.680, 0.850, 1.165, 1.301, 1.679, 2.014, 2.412, 2.690, 3.281],
    [50, 0.255, 0.433, 0.679, 0.849, 1.164, 1.299, 1.676, 2.009, 2.403, 2.678, 3.261],
    [51, 0.255, 0.433, 0.679, 0.849, 1.164, 1.299, 1.676, 2.009, 2.403, 2.678, 3.261],
    [52, 0.255, 0.433, 0.679, 0.849, 1.164, 1.299, 1.676, 2.009, 2.403, 2.678, 3.261],
    [53, 0.255, 0.433, 0.679, 0.849, 1.164, 1.299, 1.676, 2.009, 2.403, 2.678, 3.261],
    [54, 0.255, 0.433, 0.679, 0.849, 1.164, 1.299, 1.676, 2.009, 2.403, 2.678, 3.261],
    [55, 0.255, 0.433, 0.679, 0.848, 1.163, 1.297, 1.673, 2.004, 2.396, 2.668, 3.245],
    [56, 0.255, 0.433, 0.679, 0.848, 1.163, 1.297, 1.673, 2.004, 2.396, 2.668, 3.245],
    [57, 0.255, 0.433, 0.679, 0.848, 1.163, 1.297, 1.673, 2.004, 2.396, 2.668, 3.245],
    [58, 0.255, 0.433, 0.679, 0.848, 1.163, 1.297, 1.673, 2.004, 2.396, 2.668, 3.245],
    [59, 0.255, 0.433, 0.679, 0.848, 1.163, 1.297, 1.673, 2.004, 2.396, 2.668, 3.245],
    [60, 0.254, 0.433, 0.679, 0.848, 1.162, 1.296, 1.671, 2.000, 2.390, 2.660, 3.232],
    [61, 0.253, 0.431, 0.674, 0.842, 1.150, 1.282, 1.645, 1.960, 2.326, 2.576, 3.090]]

def mean(vals):
    sum = 0.0
    for i in vals:
        sum = sum + i
    return sum / len(vals)

def stddev(vals):
    if len(vals) == 1:
        return 0
    avg = mean(vals)
    sum = 0.0
    for i in vals:
        sum = sum + (i - avg) * (i - avg)
    return sum / (len(vals) - 1)

def confidence(vals):
    if len(vals) == 1:
        return (0, 0)
    mu = mean(vals)
    sigma = stddev(vals)
    E = math.sqrt(sigma) / math.sqrt(len(vals))
    if len(vals) - 1 <= 61:
        L = len(vals) - 1
    else:
        L = 61
    T = ttest[L - 1][8]
    return (mu - T * E, mu + T * E)

def parse(fname):
    f = open(fname)
    lines = f.readlines()
    f.close
    lines2 = map(lambda x: x.split("\t"), lines)
    alts = filter(lambda x: x[0] == "ALT", lines2)
    eliots = filter(lambda x: x[0] == "ELIOT", lines2)
    if len(alts) != len(eliots):
        print "Unable to compute: different lengths!"
        return ([], [], [], [])
    altacc = filter(lambda x: x[1] == "ACC", alts)
    altclock = filter(lambda x: x[1] == "CLOCK", alts)
    altimes = filter(lambda x: x[1] == "TIMES", alts)
    eliotacc = filter(lambda x: x[1] == "ACC", eliots)
    eliotclock = filter(lambda x: x[1] == "CLOCK", eliots)
    eliotimes = filter(lambda x: x[1] == "TIMES", eliots)
    acc = []
    clock = []
    usert = []
    systemt = []
    for i in range(len(altacc)):
        acc.append((int(eliotacc[i][2]) * 100) / int(altacc[i][2]) - 100)
    for i in range(len(altclock)):
        clock.append((int(eliotclock[i][2]) * 100) / int(altclock[i][2]) - 100)
    for i in range(len(altimes)):
        parts1 = eliotimes[i][2].strip("{}\n").split(",")
        parts2 = altimes[i][2].strip("{}\n").split(",")
        if int(parts2[0]) == 0:
            usert.append(int(parts1[0]) * 100)
        else:
            usert.append((int(parts1[0]) * 100) / int(parts2[0]) - 100)
        if int(parts2[1]) == 0:
            systemt.append(int(parts1[1]) * 100)
        else:
            systemt.append((int(parts1[1]) * 100) / int(parts2[1]) - 100)
    return (acc, clock, usert, systemt)

def save(fname, data):
    f = open(fname, "a")
    types = ["ACC", "CLOCK", "USERT", "SYSTEMT"]
    i = 0
    for d in data:
        f.write("PY\t" + types[i] + "\t" + str(d[0]) + "\t" + str(d[1]) + "\t" + str(d[2]) + "\n")
        i = i + 1
    f.close()

def main():
    # n = [-4.26549, -4.50909, 1.26475, 1.42241, 2.73875, 11.954, 3.61592, -9.68883, -2.96558, -3.48133]
    fname = "/home/crest/tests.txt"
    # fname = "tests.txt"
    while True:
        os.system("bash /home/crest/alt.sh")
        os.system("bash /home/crest/eliot.sh")
        lists = parse(fname)
        data = []
        for i in lists:
            d = []
            d.append(mean(i))
            d.append(stddev(i))
            d.append(confidence(i))
            data.append(d)
        save(fname, data)

if __name__ == "__main__":
    main()
