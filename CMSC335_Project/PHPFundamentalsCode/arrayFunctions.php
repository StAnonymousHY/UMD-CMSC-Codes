<!doctype html>
<html>
    <head> 
        <meta charset="utf-8" /> 
		<title>PHP Example</title>	
	</head>

	<body>
		<?php
			echo "<p>";
			$students = array("John"  => 3.4, 
							  "Mary"  => 4.0,
							  "Peter" => 1.2,
							  "Luis"  => 3.9);

			$keys = array_keys($students);
			$values = array_values($students);
			
			echo "<strong>Keys:</strong><br />";
			foreach ($keys as $entry) 
				echo "$entry<br />";  
			
			echo "<strong>Values:</strong><br />";
			// Notice the { } brackets why?	
			for ($i = 0; $i < count($students); $i++) 
				echo "{$values[$i]}<br />";  
			
			
			echo "<strong>Key/Value pairs</strong><br />";
			foreach ($students as $key => $value) 
				echo "\$students[$key]=$value<br />";
			
			$contents = print_r($students);
			echo $contents;		
			echo "</p>";
		?>		
   </body>
</html>