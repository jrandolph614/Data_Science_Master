from flask import Flask , render_template, url_for , request, redirect
from pipeline import pipe
app = Flask(__name__)


@app.route('/',methods=['GET',"POST"])
def index():
        return render_template('index.html',button_pressed=0)
@app.route('/generate',methods=['GET',"POST"]) 
def generate():
    if request.method=='POST':
        seed,path = pipe()
        return render_template('generate.html', seed=seed,path=path)
    else:
        return render_template('generate.html')
@app.route('/about')
def about():
    return render_template('about.html')


if __name__ == '__main__':
    app.run(debug=True)