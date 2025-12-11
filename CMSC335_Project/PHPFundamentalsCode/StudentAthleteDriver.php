<?php
	declare(strict_types=1);
	require("StudentAthlete.php");
	
	function printInfo(Athlete $athlete) {
		echo "<br>In function<br>";
		echo $athlete;
	}
	
	print "Creating athlete objects<br />";
	$sa1 = new StudentAthlete("Tom", "Silver Spring", 3.0, "Basketball");
	print "$sa1<hr />";
	
	$sa2 = new StudentAthlete("Rose", "College Park", 4.0, "Soccer");
	print "$sa2<hr />";
	print "Total Athletes: ".StudentAthlete::$totalAthletes."<br />";
	
	print "MaxSalary: ".StudentAthlete::maxSalary()."<br />";
	printInfo($sa1);
	
	$sa3 = new StudentAthlete("Rose", "College Park", 4.0, "Soccer");
	
	echo "<h2>Comparisons (==)</h2>";
	if ($sa2 == $sa3) {
		echo "Athletes are considered the same when comparing with ==";
	} else {
		echo "Athletes are NOT considered the same when comparing with ==.";
	}
	echo "<br/><br/>";
	
	echo "<h2>Comparisons (===)</h2>";
	if ($sa2 === $sa3) {
		echo "Athletes are considered the same when comparing with ===";
	} else {
		echo "Athletes are NOT considered the same when comparing with ===.";
	}
	echo "<br/><br/>";
?>