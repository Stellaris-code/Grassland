/*********************************************************************
Matt Marchant 2013 - 2014
SFML Tiled Map Loader - https://github.com/bjorn/tiled/wiki/TMX-Map-Format
http://trederia.blogspot.com/2013/05/tiled-map-loader-for-sfml.html

The zlib license has been used to make this software fully compatible
with SFML. See http://www.sfml-dev.org/license.php

This software is provided 'as-is', without any express or
implied warranty. In no event will the authors be held
liable for any damages arising from the use of this software.

Permission is granted to anyone to use this software for any purpose,
including commercial applications, and to alter it and redistribute
it freely, subject to the following restrictions:

1. The origin of this software must not be misrepresented;
you must not claim that you wrote the original software.
If you use this software in a product, an acknowledgment
in the product documentation would be appreciated but
is not required.

2. Altered source versions must be plainly marked as such,
and must not be misrepresented as being the original software.

3. This notice may not be removed or altered from any
source distribution.
*********************************************************************/
// map object to box2D body example.
//////////////////////////////////////////////////////////////////////

#include <SFML/System.hpp>
#include <SFML/Window.hpp>
#include <SFML/Graphics.hpp>
#include <SFML/Audio.hpp>

#include <tmx/MapLoader.h>
#include <tmx/tmx2box2d.h>

#include <tmx/DebugShape.h>

#include <Box2D/Collision/Shapes/b2PolygonShape.h>
#include <Box2D/Collision/Shapes/b2CircleShape.h>
#include <Box2D/Box2D.h>

#include <Thor/Input.hpp>


#include <memory>
#include <string>
#include <map>
#include <vector>

#include <Thor/Particles/ParticleSystem.hpp>
#include <Thor/Particles/EmissionInterface.hpp>
#include <Thor/Vectors/PolarVector.hpp>
#include <Thor/Math/Random.hpp>
#include <Thor/Time/CallbackTimer.hpp>
#include <array>
// Various constants as firework parameters
const sf::Time	explosionInterval	= sf::seconds(2.7f);
const sf::Time	explosionDuration	= sf::seconds(0.5f);
const sf::Time	tailDuration	= sf::seconds(2.5f);
const sf::Int64	tailsPerExplosion	= 60;
const float	gravity	= 30.f;
// Array with possible colors for explosions
const std::array<sf::Color, 9> fireworkColors =
{
    sf::Color(100, 255, 135),	// light green
    sf::Color(175, 255, 135),	// lime green
    sf::Color(85, 190, 255),	// light blue
    sf::Color(255, 145, 255),	// pink
    sf::Color(100, 100, 255),	// indigo
    sf::Color(140, 250, 190),	// turquoise
    sf::Color(255, 135, 135),	// red
    sf::Color(240, 255, 135),	// light yellow
    sf::Color(245, 215, 80),	// light orange
};
// Custom emitter that groups particles in tails
class FireworkEmitter
{
public:
    explicit FireworkEmitter(sf::Vector2f position)
        : mAccumulatedTime(sf::Time::Zero)
        , mPosition(position)
        , mColor(fireworkColors[thor::random(0u, fireworkColors.size() - 1u)])
    {
    }
    void operator() (thor::EmissionInterface& system, sf::Time dt)
    {
        const sf::Time tailInterval = explosionDuration / tailsPerExplosion;
        // Accumulate passed time. If enough time has passed (tailInterval), emit a new tail and decrease accumulator.
        mAccumulatedTime += dt;
        while (mAccumulatedTime - tailInterval > sf::Time::Zero)
        {
            emitTail(system);
            mAccumulatedTime -= tailInterval;
        }
    }
private:
    void emitTail(thor::EmissionInterface& system)
    {
        // Create initial direction with random vector length and angle
        thor::PolarVector2f velocity(thor::random(100.f, 200.f), thor::random(0.f, 360.f));
        // Create particle at position of explosion, with emitter-specific color and at 80% initial scale
        thor::Particle particle(tailDuration);
        particle.position = mPosition;
        particle.color = mColor;
        particle.scale *= 0.5f;
        // A tail contains 25 particles with different speeds and scales.
        // The largest particles move fastest, leading to a comet-like tail effect.
        for (sf::Int64 i = 0; i < 50; ++i)
        {
            // Decrease scale continuously
            particle.scale *= 0.95f;
            // Decrease speed continuously
            velocity.r *= 0.96f;
            particle.velocity = velocity;
            // Add adapted particle to particle system
            system.emitParticle(particle);
        }
    }
private:
    sf::Time	mAccumulatedTime;
    sf::Vector2f	mPosition;
    sf::Color	mColor;
};
// Custom affector that fades particles out and accelerates them according to scale
class FireworkAffector
{
public:
    void operator() (thor::Particle& particle, sf::Time dt)
    {
        // Apply gravity, where particles with greater scale are affected stronger (quadratic influence)
        particle.velocity += dt.asSeconds() * sf::Vector2f(0.f, gravity) * particle.scale.x * particle.scale.y;
        // Let particles continuously fade out (particles with smaller scale have already lower alpha value at beginning)
        particle.color.a = static_cast<sf::Uint8>(256 * thor::getRemainingRatio(particle) * particle.scale.x);
    }
};

constexpr const char* waterShader()
{
    return
            "#version 120\n"
            "uniform sampler2D baseTexture;"
            "uniform float time = 1.0;" //time in seconds

            //
            // Description : Array and textureless GLSL 2D/3D/4D simplex
            //               noise functions.
            //      Author : Ian McEwan, Ashima Arts.
            //  Maintainer : ijm
            //     Lastmod : 20110822 (ijm)
            //     License : Copyright (C) 2011 Ashima Arts. All rights reserved.
            //               Distributed under the MIT License. See LICENSE file.
            //               https://github.com/ashima/webgl-noise
            //

            "vec3 mod289(vec3 x) {"
            "return x - floor(x * (1.0 / 289.0)) * 289.0;"
            "}"

            "vec4 mod289(vec4 x) {"
            "return x - floor(x * (1.0 / 289.0)) * 289.0;"
            "}"

            "vec4 permute(vec4 x) {"
            "return mod289(((x*34.0) + 1.0)*x);"
            "}"

            "vec4 taylorInvSqrt(vec4 r)"
            "{"
            "return 1.79284291400159 - 0.85373472095314 * r;"
            "}"

            "float snoise(vec3 v)"
            "{"
            "const vec2  C = vec2(1.0 / 6.0, 1.0 / 3.0);"
            "const vec4  D = vec4(0.0, 0.5, 1.0, 2.0);"

            // First corner
            "vec3 i = floor(v + dot(v, C.yyy));"
            "vec3 x0 = v - i + dot(i, C.xxx);"

            // Other corners
            "vec3 g = step(x0.yzx, x0.xyz);"
            "vec3 l = 1.0 - g;"
            "vec3 i1 = min(g.xyz, l.zxy);"
            "vec3 i2 = max(g.xyz, l.zxy);"

            //   x0 = x0 - 0.0 + 0.0 * C.xxx;
            //   x1 = x0 - i1  + 1.0 * C.xxx;
            //   x2 = x0 - i2  + 2.0 * C.xxx;
            //   x3 = x0 - 1.0 + 3.0 * C.xxx;
            "vec3 x1 = x0 - i1 + C.xxx;"
            "vec3 x2 = x0 - i2 + C.yyy;" // 2.0*C.x = 1/3 = C.y
            "vec3 x3 = x0 - D.yyy;"      // -1.0+3.0*C.x = -0.5 = -D.y

            // Permutations
            "i = mod289(i);"
            "vec4 p = permute(permute(permute("
            "i.z + vec4(0.0, i1.z, i2.z, 1.0))"
            "+ i.y + vec4(0.0, i1.y, i2.y, 1.0))"
            "+ i.x + vec4(0.0, i1.x, i2.x, 1.0));"

            // Gradients: 7x7 points over a square, mapped onto an octahedron.
            // The ring size 17*17 = 289 is close to a multiple of 49 (49*6 = 294)
            "float n_ = 0.142857142857;" // 1.0/7.0
            "vec3  ns = n_ * D.wyz - D.xzx;"

            "vec4 j = p - 49.0 * floor(p * ns.z * ns.z);"  //  mod(p,7*7)

            "vec4 x_ = floor(j * ns.z);"
            "vec4 y_ = floor(j - 7.0 * x_);"    // mod(j,N)

            "vec4 x = x_ *ns.x + ns.yyyy;"
            "vec4 y = y_ *ns.x + ns.yyyy;"
            "vec4 h = 1.0 - abs(x) - abs(y);"

            "vec4 b0 = vec4(x.xy, y.xy);"
            "vec4 b1 = vec4(x.zw, y.zw);"

            //vec4 s0 = vec4(lessThan(b0,0.0))*2.0 - 1.0;
            //vec4 s1 = vec4(lessThan(b1,0.0))*2.0 - 1.0;
            "vec4 s0 = floor(b0)*2.0 + 1.0;"
            "vec4 s1 = floor(b1)*2.0 + 1.0;"
            "vec4 sh = -step(h, vec4(0.0));"

            "vec4 a0 = b0.xzyw + s0.xzyw*sh.xxyy;"
            "vec4 a1 = b1.xzyw + s1.xzyw*sh.zzww;"

            "vec3 p0 = vec3(a0.xy, h.x);"
            "vec3 p1 = vec3(a0.zw, h.y);"
            "vec3 p2 = vec3(a1.xy, h.z);"
            "vec3 p3 = vec3(a1.zw, h.w);"

            //Normalise gradients
            "vec4 norm = taylorInvSqrt(vec4(dot(p0, p0), dot(p1, p1), dot(p2, p2), dot(p3, p3)));"
            "p0 *= norm.x;"
            "p1 *= norm.y;"
            "p2 *= norm.z;"
            "p3 *= norm.w;"

            // Mix final noise value
            "vec4 m = max(0.6 - vec4(dot(x0, x0), dot(x1, x1), dot(x2, x2), dot(x3, x3)), 0.0);"
            "m = m * m;"
            "return 42.0 * dot(m*m, vec4(dot(p0, x0), dot(p1, x1),"
            "dot(p2, x2), dot(p3, x3)));"
            "}"


            "void main(void)"
            "{"
            //sin distort for ripples
            "const float sinWidth = 0.08;" //smaller is wider
            "const float sinHeight = 0.001;" //larger is taller
            "const float sinTime = 9.5;" //larger is faster (if time is input in seconds then this value hertz)

            "vec2 coord = gl_TexCoord[0].xy;"

            "float offsetX = sin(gl_FragCoord.y * sinWidth + time * sinTime) * 0.5 + 0.5;"
            "coord.x += offsetX * sinHeight / 2.0;"

            "float offsetY = sin(gl_FragCoord.x * sinWidth + time * sinTime) * 0.5 + 0.5;"
            "coord.y += offsetY * sinHeight;"

            "float highlight = snoise(vec3(gl_FragCoord.xy * 0.04, time)) * 0.1;"

            //add colour for highlight
            "vec3 colour = texture2D(baseTexture, coord).rgb;"
            "colour = clamp(colour + highlight, 0.0, 1.0);"
            "gl_FragColor = vec4(colour, 1.0);"
            "}";
}


bool checkIfContact(const b2Body* body)
{
    for (const b2ContactEdge* c = body->GetContactList(); c != nullptr; c = c->next)
        if(c->contact->IsTouching())
            return true;

    return false;
}


int main()
{
    thor::Action actionJump(sf::Keyboard::Up, thor::Action::Hold);
    thor::Action actionMoveR(sf::Keyboard::Right, thor::Action::Hold);
    thor::Action actionMoveL(sf::Keyboard::Left, thor::Action::Hold);
    thor::Action actionRestart(sf::Keyboard::R, thor::Action::PressOnce);



    constexpr const char* water_shader = waterShader();

    sf::Clock shaderClock, pClock;

    sf::Music mainTheme;
    mainTheme.openFromFile("SuperMarioBros.ogg");
    mainTheme.setLoop(true);
    sf::Music dieSound;
    dieSound.openFromFile("smb_mariodie.ogg");
    dieSound.setLoop(false);
    sf::Music stageClear;
    stageClear.openFromFile("smb_stage_clear.ogg");
    stageClear.setLoop(false);
    sf::SoundBuffer fireBuffer;
    if (!fireBuffer.loadFromFile("fireworks.ogg"))
        return -1;

    sf::Sound fireSound;
    fireSound.setBuffer(fireBuffer);

    sf::Shader waterEffect;
    sf::Vector2f originalPos;
    waterEffect.loadFromMemory(water_shader, sf::Shader::Fragment);


    sf::RenderWindow renderWindow(sf::VideoMode(25 * 32, 13 * 32), "Neo Mario");
    renderWindow.setVerticalSyncEnabled(true);

    sf::Texture particleTex;
    if (!particleTex.loadFromFile("particle.png"))
        return EXIT_FAILURE;

    thor::ActionMap<std::string> actionMap;

    actionMap["jump"] = actionJump;
    actionMap["moveRight"] = actionMoveR;
    actionMap["moveLeft"] = actionMoveL;
    actionMap["restart"] = actionRestart;

    thor::ParticleSystem pSystem;
    pSystem.setTexture(particleTex);
    pSystem.addAffector(FireworkAffector());

    thor::CallbackTimer explosionTimer;
    explosionTimer.restart(sf::seconds(1.f));
    // Connect timer to a lambda expression which restarts the timer every time it expires
    explosionTimer.connect( [] (thor::CallbackTimer& trigger)
    {
        trigger.restart(explosionInterval);
    });
    // Connect timer to a lambda expression that creates an explosion at expiration
    explosionTimer.connect0( [&pSystem, &renderWindow] ()
    {
        // Compute random position on screen
        sf::Vector2f position(thor::randomDev(renderWindow.getSize().x/2.0f, 200),thor::randomDev(renderWindow.getSize().y/2.0f-100, 100));
        // Add a temporary emitter to the particle system
        pSystem.addEmitter(FireworkEmitter(position), explosionDuration);
    });

    //create map loader and load map
    tmx::MapLoader ml("maps/");
    ml.Load("mario1.tmx");

    ml.SetLayerShader(0u, waterEffect);


    //create a box2D world
    b2World world(tmx::SfToBoxVec(sf::Vector2f(0.f, 1000.f)));

    sf::Texture mario;
    mario.loadFromFile("mario.png");

    sf::Vertex backGradient[] =
    {
        sf::Vertex(sf::Vector2f(0, 0), sf::Color::Blue),
        sf::Vertex(sf::Vector2f(renderWindow.getSize().x, 0), sf::Color::Blue),
        sf::Vertex(sf::Vector2f(renderWindow.getSize()), sf::Color(102, 255, 255)),
        sf::Vertex(sf::Vector2f(0, renderWindow.getSize().y), sf::Color(102, 255, 255))
    };


    //parse map objects
    std::vector<std::unique_ptr<sf::Shape>> debugBoxes;
    std::vector<DebugShape> debugShapes;
    std::map<b2Body*, sf::CircleShape> dynamicShapes; //we can use raw pointers because box2D manages its own memory

    sf::Font font;
    font.loadFromFile("fonts/Ubuntu-M.ttf");
    sf::Text win_text("You win !", font);
    win_text.setColor(sf::Color::Green);
    sf::Text fail_text("Game over", font);
    fail_text.setColor(sf::Color::Red);
    sf::Text restart_text("(Press R to restart)", font);

    sf::FloatRect win_rect = win_text.getLocalBounds();
    win_text.setOrigin(win_rect.left + win_rect.width/2.0f,
                       win_rect.top  + win_rect.height/2.0f);
    win_text.setPosition(sf::Vector2f(renderWindow.getSize().x/2.0f,renderWindow.getSize().y/2.0f));

    sf::FloatRect fail_rect = fail_text.getLocalBounds();
    fail_text.setOrigin(fail_rect.left + fail_rect.width/2.0f,
                        fail_rect.top  + fail_rect.height/2.0f);
    fail_text.setPosition(sf::Vector2f(renderWindow.getSize().x/2.0f,renderWindow.getSize().y/2.0f));

    sf::FloatRect restart_rect = restart_text.getLocalBounds();
    restart_text.setOrigin(restart_rect.left + restart_rect.width/2.0f,
                           restart_rect.top  + restart_rect.height/2.0f - 30);
    restart_text.setPosition(sf::Vector2f(renderWindow.getSize().x/2.0f,renderWindow.getSize().y/2.0f));


    std::map<std::string, tmx::MapObject> importantPoints; // checkpoints, finish... But as readOnly
    std::vector<tmx::MapObject> pitfalls;
    sf::CircleShape sh;

    const std::vector<tmx::MapLayer>& layers = ml.GetLayers();
    bool playingDie = false;
    bool playingWin = false;

    for (const auto& l : layers)
    {
        if (l.name == "Static") //static bodies which make up the map geometry
        {
            for (const auto& o : l.objects)
            {
                //receive a pointer to the newly created body
                b2Body* b = tmx::BodyCreator::Add(o, world);

                //iterate over body info to create some visual debugging shapes to help visualise
                debugBoxes.push_back(std::unique_ptr<sf::RectangleShape>(new sf::RectangleShape(sf::Vector2f(6.f, 6.f))));
                sf::Vector2f pos = tmx::BoxToSfVec(b->GetPosition());
                debugBoxes.back()->setPosition(pos);
                debugBoxes.back()->setOrigin(3.f, 3.f);

                for (b2Fixture* f = b->GetFixtureList(); f; f = f->GetNext())
                {
                    b2Shape::Type shapeType = f->GetType();
                    if (shapeType == b2Shape::e_polygon)
                    {
                        DebugShape ds;
                        ds.setPosition(pos);
                        b2PolygonShape* ps = (b2PolygonShape*)f->GetShape();

                        int count = ps->GetVertexCount();
                        for (int i = 0; i < count; i++)
                            ds.AddVertex(sf::Vertex(tmx::BoxToSfVec(ps->GetVertex(i)), sf::Color::Green));

                        ds.AddVertex(sf::Vertex(tmx::BoxToSfVec(ps->GetVertex(0)), sf::Color::Green));
                        debugShapes.push_back(ds);
                    }
                    else if (shapeType == b2Shape::e_circle)
                    {
                        b2CircleShape* cs = static_cast<b2CircleShape*>(f->GetShape());
                        float radius = tmx::BoxToSfFloat(cs->m_radius);
                        std::unique_ptr<sf::CircleShape> c(new sf::CircleShape(radius));
                        c->setPosition(pos);
                        c->setOrigin(radius, radius);
                        c->setOutlineColor(sf::Color::Green);
                        c->setOutlineThickness(-1.f);
                        c->setFillColor(sf::Color::Transparent);
                        debugBoxes.push_back(std::move(c));
                    }
                }
            }
        }
        else if (l.name == "Dynamic")
        {
            for (const auto& o : l.objects)
            {
                //this time keep a copy of the pointer so we can update the dynamic objects
                //with their information. Don't forget to create a dynamic body
                b2Body* b = tmx::BodyCreator::Add(o, world, b2BodyType::b2_dynamicBody);
                if (o.GetName() == "player_start")
                {
                    b->GetFixtureList()->SetRestitution(0.f); //set some properties of the body
                    originalPos = o.GetPosition();
                    std::cout << originalPos.x << originalPos.y << std::endl;
                }
                //we assume for this example all dynamic objects are circular. Other shapes also work
                //but you need to impliment your own drawing for them.
                b2CircleShape* cs = static_cast<b2CircleShape*>(b->GetFixtureList()->GetShape());

                sh.setTexture(&mario);
                const float radius = tmx::BoxToSfFloat(cs->m_radius);
                sh.setRadius(radius);
                sh.setOrigin(radius,radius);
                //sh.setPosition(tmx::BoxToSfVec(b->GetPosition()));

                dynamicShapes.insert(std::pair<b2Body*, sf::CircleShape>(b, sh));
            }
        }
        else if (l.name == "Pitfalls")
        {
            for (const auto& i : l.objects)
            {
                pitfalls.push_back(i);
            }
        }
        else if (l.name == "MapPoints")
        {
            for (const auto& i : l.objects)
            {
                importantPoints[i.GetName()] = i;
            }
        }
    }


    //-----------------------------------//
    sf::Clock clock;
    sf::Clock jumpClock;
    bool jumpClockActive = false;
    bool died = false;
    bool won = false;

    mainTheme.play();
    while(renderWindow.isOpen())
    {
        pSystem.update(pClock.restart());
        explosionTimer.update();

        waterEffect.setParameter("time", shaderClock.getElapsedTime().asSeconds());
        actionMap.clearEvents();
        //poll input
        sf::Event event;
        while(renderWindow.pollEvent(event))
        {
            actionMap.pushEvent(event);
            if (event.type == sf::Event::Closed)
                renderWindow.close();
        }
        //update
        world.Step(clock.restart().asSeconds(), 6, 3);
        sf::CircleShape player;
        //dynamicShapes.
        for (auto& ds : dynamicShapes)
        {
            if (!won && !died)
            {
                const b2Vec2 velocity = ds.first->GetLinearVelocity();
                if (actionMap.isActive("moveRight"))
                {

                    ds.first->SetLinearVelocity(b2Vec2(3,velocity.y));

                }
                else if (actionMap.isActive("moveLeft"))
                {
                    ds.first->SetLinearVelocity(b2Vec2(-3,velocity.y));
                }
                else
                {
                    ds.first->SetLinearVelocity(b2Vec2(0,velocity.y));
                }

                if (actionMap.isActive("jump") && checkIfContact(ds.first) && std::abs(velocity.y) < 0.01)
                {
                    ds.first->ApplyLinearImpulse(tmx::SfToBoxVec(sf::Vector2f(0,-13)), ds.first->GetWorldCenter(), false);

                }
                ds.second.setPosition(tmx::BoxToSfVec(ds.first->GetPosition()));
                player = ds.second;
            }
            if (actionMap.isActive("restart"))
            {
                player.setPosition(originalPos);
                ds.first->SetTransform(tmx::SfToBoxVec(originalPos),ds.first->GetAngle());
                died = false;
                won = false;
                mainTheme.stop();
                mainTheme.play();
                dieSound.stop();
                stageClear.stop();
                playingDie = false;
                playingWin = false;
            }
        }
        //draw
        renderWindow.clear();
        renderWindow.draw(backGradient,4,sf::Quads);
        renderWindow.draw(ml);
        for (auto& ds : dynamicShapes)
            renderWindow.draw(ds.second);

        for (const auto& pit : pitfalls)
        {
            if (pit.Contains(player.getPosition()))
            {
                died = true;
            }


        }
        if (importantPoints["map_finish"].Contains(player.getPosition()))
        {
            won = true;
        }

        if (won)
        {
            renderWindow.draw(win_text);
            renderWindow.draw(restart_text);
            renderWindow.draw(pSystem, sf::BlendAdd);
            mainTheme.stop();
            if (!playingWin)
            {
                stageClear.play();

                playingWin = true;
            }
            if (fireSound.getStatus() != sf::Sound::Playing)
                fireSound.play();

        }
        else if (died)
        {
            renderWindow.draw(fail_text);
            renderWindow.draw(restart_text);
            mainTheme.stop();
            if (!playingDie)
            {
                dieSound.play();
                playingDie = true;
            }

        }

        renderWindow.display();
    }

    return 0;
}

