
function global:dev_env_shared() {
    set-item Env:CLOUDAMQP_URL amqp://guest:guest@localhost
    set-item Env:DATABASE_URL postgres://uopgqzjhykamka:aa174d7296ea8bf85200a03c34d25b4f397a701400f659510f72f73bf44477bf@ec2-54-247-95-125.eu-west-1.compute.amazonaws.com:5432/d2stq3d5c6ofrt
    set-item Env:MONGODB_URI mongodb://heroku_fwl4nhng:b0mlv19vnevpr31d602n064avm@ds249839.mlab.com:49839/heroku_fwl4nhng
    set-item Env:REDIS_URL redis://h:p343a5f6483b3bd68f5eaccab4581ed84f3c5b8e9ca41e3640f7cea1de61eb2e3@ec2-52-18-191-147.eu-west-1.compute.amazonaws.com:29469
    set-item Env:SENDGRID_PASSWORD fch3whdb1913
    set-item Env:SENDGRID_USERNAME app94409464@heroku.com
}

function global:dev_email() {
    dev_env_shared
    set-item Env:NUM_SENDERS 1
    stack exec hud-email
}

function global:dev_identity() {
    dev_env_shared
    set-item Env:PORT 8080
    set-item Env:NUM_VERIFIERS 1
    set-item Env:NUM_SIGNERS 1
    set-item Env:SENDER_ADDRESS noreply@hud.io
    set-item Env:HMAC_KEY FNkTEtVmzwIdOSiCpdFj3dhaNogSW3BgtwFErFzxk6-bzVkuWuaGUDh4ABV1WjnB
    stack exec hud-identity
}

function global:dev_github() {
    dev_env_shared
    set-item Env:NUM_SERVERS 1
    set-item Env:NUM_AUTHORISERS 1
    set-item Env:GITHUB_CLIENT_ID abf01a6688c17bbae0f5
    set-item Env:GITHUB_CLIENT_SECRET 290104214486f9c01b7c9ccc41bdab439e9d2ad2
    stack exec hud-github
}

function global:dev_trello() {
    dev_env_shared
    set-item Env:NUM_SERVERS 1
    set-item Env:NUM_AUTHORISERS 1
    set-item Env:TRELLO_CLIENT_KEY 407114f0fb91ba30d564992575192849
    stack exec hud-trello
}

function global:dev_heroku() {
    dev_env_shared
    set-item Env:NUM_SERVERS 1
    set-item Env:NUM_AUTHORISERS 1
    set-item Env:HEROKU_CLIENT_ID 07714456-81f9-4fed-a678-4366f300c37d
    set-item Env:HEROKU_CLIENT_SECRET 8a5060a6-225f-4855-915b-231ade7629a9
    stack exec hud-trello
}

function global:dev_dashboard() {
    dev_env_shared
    set-item Env:PORT 8081
    stack exec hud-dashboard
}
