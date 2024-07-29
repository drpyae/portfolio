document.querySelectorAll('a[href^="#"]').forEach(anchor => {
    anchor.addEventListener('click', function(e) {
        e.preventDefault();
        document.querySelector(this.getAttribute('href')).scrollIntoView({
            behavior: 'smooth'
        });
    });
});
// Back to top button
const backToTopButton = document.getElementById('back-to-top');

window.onscroll = function() {
    if (document.body.scrollTop > 20 || document.documentElement.scrollTop > 20) {
        backToTopButton.style.display = "block";
    } else {
        backToTopButton.style.display = "none";
    }
};

backToTopButton.addEventListener('click', function() {
    document.body.scrollTop = 0;
    document.documentElement.scrollTop = 0;
});

document.getElementById('contact-form').addEventListener('submit', function(event) {
    event.preventDefault();
    const name = document.getElementById('name').value;
    const email = document.getElementById('email').value;
    const message = document.getElementById('message').value;
    const status = document.getElementById('form-status');

    if (name && email && message) {
        emailjs.send(service_vsi7v4j, template_jap6fg5, {
            from_name: name,
            from_email: email,
            message: message
        }).then(function(response) {
            status.textContent = 'Thank you for your message!';
            status.style.color = 'green';
        }, function(error) {
            status.textContent = 'Failed to send message. Please try again later.';
            status.style.color = 'red';
        });
    } else {
        status.textContent = 'Please fill in all fields.';
        status.style.color = 'red';
    }
});
fetch('blog-posts.json')
    .then(response => response.json())
    .then(data => {
        const blogSection = document.getElementById('blog');
        data.forEach(post => {
            const postDiv = document.createElement('div');
            postDiv.className = 'blog-post';
            postDiv.innerHTML = `
                <h3>${post.title}</h3>
                <p>Date: ${post.date}</p>
                <p>${post.excerpt}</p>
                <a href="${post.url}" target="_blank">Read More</a>
            `;
            blogSection.appendChild(postDiv);
        });
    })
    .catch(error => console.error('Error fetching blog posts:', error));
