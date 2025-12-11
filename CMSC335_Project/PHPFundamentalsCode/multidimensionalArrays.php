<!doctype html>
<html>
    <head> 
        <meta charset="utf-8" /> 
		<title>PHP Example</title>	
	</head>

	<body>
		<?php
			echo "<p>";
			print("Numerical Example<br />");
			$gradebook[0][0] = 10;
			$gradebook[0][1] = 20;
			$gradebook[0][2] = 30;

			$gradebook[1][0] = 40;
			$gradebook[1][1] = 50;
			$gradebook[1][2] = 60;

			for ($row = 0; $row < 2; $row++) {
				// We need the curly brackets inside the echo
				for ($col = 0; $col < 3; $col++) {
					echo "{$gradebook[$row][$col]} ";
				}
				print("<br />");
			}

			$courses = array("cmsc103" => array("teacher" => "John", "time" => "11am"),
						  	 "cmsc214" => array("teacher" => "Mary", "time" => "3pm"));
						 
			$courses["cmsc320"] = array("teacher" => "Peter", "time" => "8am");
			
			$keys = array_keys($courses);
			echo "Courses:<br />";
			foreach ($keys as $entry)
			 	echo $entry."<br />";
			
			echo "All Information:<br />";
			while (list($courseName, $course) = each($courses)) {
				echo "$courseName, {$course["teacher"]}, {$course["time"]}<br />";
			}
			
			echo "</p>";
		?>		
   </body>
</html>