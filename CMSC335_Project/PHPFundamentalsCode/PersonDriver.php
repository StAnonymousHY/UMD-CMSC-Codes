<?php
	declare(strict_types=1);
	
	# making class accessible 
	require("Person.php");
	
	/**** Driver ****/
	$p1 = new Person("John", "Far from here");
	$p2 = new Person("Mary", "College Park");
	echo "getName: ", $p1->getName(), "<br/>";
	echo "getAddress: ", $p1->getAddress(), "<br/>";
	echo "__toString: ", $p1, "<br/>";
	$p1->getInfo();
	echo "<br><br><strong>About to perform var_dump()</strong><br>";
	var_dump($p1);
	echo "<br><strong>End of var_dump()</strong><br>";
	var_dump($p2);
?>