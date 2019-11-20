---
title: Courses
---

## Teaching 

Rosette is used for teaching in the following courses and tutorials on
solver-aided programming and, more generally, computer-aided reasoning for
software. Contact [our team][UNSAT contact] if you would like to use our teaching
materials (starred) in your own course, or have your course listed here. 


### Courses

{% for course in site.data.courses %}
| {% if course.unsat %}&#9734; {% endif %} [{{course.course}}]({{course.url}}): {{course.title}} | {{course.institution}} | {% endfor %}

### Tutorials

{% for tutorial in site.data.tutorials %}
| {% if tutorial.unsat %}&#9734; {% endif %} [{{tutorial.title}}]({{tutorial.url}}) at {% for event in tutorial.events %}{% if forloop.index0 > 0 %}, {% endif %}[{{event.venue}}]({{event.url}}){% endfor %}| {% endfor %}



[UNSAT contact]: https://unsat.cs.washington.edu/index.html#contact
