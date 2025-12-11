<!doctype html>
<html>
    <head> 
        <meta charset="utf-8" /> 
		<title>PHP Example</title>	
	</head>

	<body>
		<h1>Hello</h1>
		<?php
      		// This is a comment in php
			# this is also a comment
			/*
				Multiple lines comment
			*/
			echo "<h1>This is the first example showing generation of html</h1>";
			echo '<h2>This is the second example showing generation of html</h2>';
			echo 'Mary', 'John';
			print("<p>We can use either <strong>echo or print</strong> to generate a message</p>");
 			print '<p>Parenthesis not needed and string can be in single quotes</p>';
		?>
      
		<p>This is just html </p>
      
		<?php
			echo "<p>Back to PHP</p>";
		?>
   </body>
</html>