<?php
	require("Student.php");
	
	print "Creating a student object<br />";
	$s1 = new Student("Rose", "Bethesda", 3.6);
	print "$s1<hr />";
	Student::printMAXGPA();
	print "<hr />";
?>