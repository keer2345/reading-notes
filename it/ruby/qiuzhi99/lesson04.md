# Lesson 04

*app/views/home/index.html.erb*

``` html+erb
<nav class="nav">
	<ul>
		<li><%= link_to "Blog", nil %></li>
		<li>
			<%= link_to "Guides", "https://guides.rubyonrails.org/", target: "_blank"
			%>
		</li>
		<li>
			<%= link_to "API", "https://api.rubyonrails.org/", target: "_blank" %>
		</li>
	</ul>
	<%= link_to root_path, class:"logo" do %> <%= image_tag "logo.svg" %> <% end
	%>
	<ul>
		<li>
			<%= link_to "Forum", "https://discuss.rubyonrails.org/", target: "_blank"
			%>
		</li>
		<li>
			<%= link_to "Contribute", "https://github.com/rails/rails", target:
			"_blank" %>
		</li>
		<li>
			<%= link_to "Team", "https://rubyonrails.org/community", target: "_blank"
			%>
		</li>
	</ul>
</nav>

<div class="hero">
	<div class="container">
		<h1>Compress the complexity of modern web apps.</h1>
		<h4>
			Learn just what you need to get started, then keep leveling up as you go.
			<strong>Ruby on Rails scales from HELLO WORLD to IPO.</strong>
		</h4>
		<div class="button">
			<%= link_to "Rails 7.0.2.2 — released February 11, 2022", nil %>
		</div>
	</div>
</div>

<div class="video">
	<div class="container">
		<%= video_tag "/Rails7.mp4",
		poster: "rails7-screencast-poster.jpg", controls:true %>
	</div>
</div>

```

*app/assets/stylesheets/style.css*

``` css
/* ... lesson 03 */
.hero {
	display: flex;
	justify-content: center;
	align-items: center;
}
.hero .container {
	max-width: 1080px;
}
.hero h1 {
	color: #d30001;
	font-weight: 700;
	font-size: 65px;
	line-height: 72px;
	text-align: center;
}

.hero h4 {
	font-size: 36px;
	color: #261b23;
	letter-spacing: -0.01rem;
	line-height: 44px;
	text-align: center;
	font-weight: 400;
}

.hero h4 strong {
	color: #261b23;
	font-weight: 700;
}
.hero .button {
	display: flex;
	justify-content: center;
	align-items: center;
}

.hero .button a {
	background: #f0e7e9;
	border-radius: 6px;
	color: #261b23;
	padding: 10px 20px;
	text-decoration: none;
	font-size: 20px;
	font-weight: 600;
}
.hero .button a:hover {
	background: #dcd3d5;
	color: #d30001;
}

.video {
	display: flex;
	justify-content: center;
	align-items: center;
	margin-top: 60px;
	background: #f0e7e9;
	position: relative;
}

.video:before {
	background-position: left top;
	background-repeat: no-repeat;
	background-size: 100% 100%;
	content: "";
	height: 10vw;
	left: 0;
	max-height: 160px;
	position: absolute;
	top: -1px;
	width: 100%;
	background-image: url("shape-top-down-left.svg");
	filter: brightness(0) invert(1);
}

.video .container {
	max-width: 1080px;
	z-index: 1;
}

.video .container video {
	border-radius: 8px;
	box-shadow: 0 15px 70px 5px rgb(38 27 35 / 15%), 0 1px 1px rgb(38 27 35 / 4%);
	max-width: 100%;
	overflow: hidden;
}

```

*app/assets/images/shape-top-down-left.svg*

``` svg
<svg preserveAspectRatio="none" width="1600" height="160" viewBox="0 0 1600 160" version="1.1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"><title>shape-top-down-left</title><g stroke="none" stroke-width="1" fill="none" fill-rule="evenodd"><path d="M0,140.00072 L0,0.000720438065 L1600,0.000720438065 C1136.32007,145.714253 610,190.57072 0,140.00072 Z" fill="#000"/></g></svg>
```

*Rails7.mp4*

Download *Rails7.mp4* from https://d1snj8sshb5u7m.cloudfront.net/Rails7.mp4 .

*.gitignore*

``` shell
/public/*.mp4
```
