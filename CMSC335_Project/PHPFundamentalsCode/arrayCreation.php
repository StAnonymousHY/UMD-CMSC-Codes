<!doctype html>
<html>
    <head> 
        <meta charset="utf-8" /> 
		<title>PHP Example</title>	
	</head>

	<body>
		<?php
			echo "<p>";
			$data[1] = 10;
			$data[2] = 20;
			echo "Values are: $data[1] and $data[2]<br />";
			
			$languages = array("C++", "Java", "Fortran");
			echo "First Value is $languages[0]<br />";
			
			$dictionary = array("House" => "Casa", "Friend" => "Amigo");
			foreach ($dictionary as $entry) 
				echo "$entry<br />";
			
				
			echo "<strong>All Languages</strong><br />";
			foreach ($languages as $entry) {
				echo "$entry<br />";
			}
			
			$mixed[] = "Hello";
			$mixed[] = 20;
			$mixed[] = 4.5;
			echo "<strong>Mixed Contents</strong><br >";
			foreach ($mixed as $entry) 
				echo "$entry<br />";
			
			echo "</p>";
		?>		
   </body>
</html>