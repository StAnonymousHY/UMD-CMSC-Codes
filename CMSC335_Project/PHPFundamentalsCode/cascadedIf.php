<!doctype html>
<html>
    <head> 
        <meta charset="utf-8" /> 
		<title>PHP Example</title>	
	</head>

	<body>
		<?php
			echo "<p>";
			$numericGrade = 70;

			if ($numericGrade >= 90.0)
				$letterGrade = "A";
			elseif ($numericGrade >= 80.0)
				$letterGrade = "B";
			elseif ($numericGrade >= 70.0)
				$letterGrade = "C";
			elseif ($numericGrade >= 60.0)
				$letterGrade = "D";
			else
				$letterGrade = "F";

			print("LetterGrade is: $letterGrade");
			echo "</p>";
		?>
   </body>
</html>