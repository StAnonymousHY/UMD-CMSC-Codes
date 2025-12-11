<!doctype html>
<html>
    <head> 
        <meta charset="utf-8" /> 
		<title>PHP Example</title>	
	</head>

	<body>
		<?php
			echo createHeader("Fear the Turtle");
			echo createHeader();
		?>		
   </body>
</html>

<?php
	function createHeader($header = "Welcome to College Park"){
		return "<h1>".$header."</h1>";
	}
?>