bl_info = {
    "name": "Regenesis Building System",
    "author": "Nomadic_Jester",
    "version": (1, 5, 3),
    "blender": (3, 0, 0),
    "location": "3D View",
    "description": "Rust-like 3m building with triangle edge snapping, working ceilings, ghostly blue preview + multi-placement (LMB place & continue, RMB/ESC cancel)",
    "category": "3D View",
}

from math import atan2, radians

import bmesh
import bpy
from bpy_extras import view3d_utils
from mathutils import Matrix, Vector

UNIT = 3.0
TRI_H = (3**0.5) / 2 * UNIT
THICK = 0.2

FOUNDATION_NAMES = ("Square_Foundation", "Tri_Foundation")





# ─── Materials ────────────────────────────────────────────────────────────────

def ensure_preview_material():
    name = "Regenesis_Preview_Ghost"
    if name in bpy.data.materials:
        return bpy.data.materials[name]

    mat = bpy.data.materials.new(name)
    mat.use_nodes = True
    nodes = mat.node_tree.nodes
    links = mat.node_tree.links

    nodes.clear()

    output   = nodes.new(type='ShaderNodeOutputMaterial')
    emission = nodes.new(type='ShaderNodeEmission')
    transp   = nodes.new(type='ShaderNodeBsdfTransparent')
    mix      = nodes.new(type='ShaderNodeMixShader')

    emission.inputs[0].default_value = (0.45, 0.75, 1.0, 1.0)
    emission.inputs[1].default_value = 0.4

    links.new(emission.outputs[0], mix.inputs[1])
    links.new(transp.outputs[0],   mix.inputs[2])
    links.new(mix.outputs[0],      output.inputs[0])

    mix.inputs[0].default_value = 0.35

    mat.blend_method = 'BLEND'
    mat.use_transparent_shadows = True
    mat.use_backface_culling = False

    mat.diffuse_color = (0.45, 0.75, 1.0, 0.25)
    mat.show_transparent_back = True
    mat.use_screen_refraction = True

    return mat


def ensure_solid_material():
    name = "Regenesis_Default_Solid"
    if name in bpy.data.materials:
        return bpy.data.materials[name]

    mat = bpy.data.materials.new(name)
    mat.use_nodes = True

    bsdf = mat.node_tree.nodes.get("Principled BSDF")
    if bsdf:
        bsdf.inputs[0].default_value = (0.85, 0.85, 0.85, 1.0)
        bsdf.inputs[7].default_value = 0.1

    mat.blend_method = 'OPAQUE'

    return mat


# ---------------- meshes ----------------

def create_mesh_object(name, verts, faces):
    mesh = bpy.data.meshes.new(name)
    mesh.from_pydata(verts, [], faces)
    mesh.update()
    obj = bpy.data.objects.new(name, mesh)
    bpy.context.collection.objects.link(obj)
    return obj


def extrude_solid(obj, thickness, along="Z"):
    me = obj.data
    bm = bmesh.new()
    bm.from_mesh(me)
    bmesh.ops.recalc_face_normals(bm, faces=bm.faces)
    geom = bmesh.ops.extrude_face_region(bm, geom=bm.faces)["geom"]
    verts = [e for e in geom if isinstance(e, bmesh.types.BMVert)]

    if along == "Z":
        offset = Vector((0, 0, thickness))
    elif along == "Y":
        offset = Vector((0, thickness, 0))
    else:
        offset = Vector((0, 0, thickness))

    for v in verts:
        v.co += offset

    bm.to_mesh(me)
    bm.free()


def make_square_foundation():
    s = UNIT / 2.0
    verts = [
        Vector((-s, -s, 0)),
        Vector((s, -s, 0)),
        Vector((s, s, 0)),
        Vector((-s, s, 0)),
    ]
    obj = create_mesh_object("Square_Foundation", verts, [(0, 1, 2, 3)])
    extrude_solid(obj, THICK, "Z")
    return obj


def make_tri_foundation():
    verts = [
        Vector((-UNIT / 2, 0, 0)),
        Vector((UNIT / 2, 0, 0)),
        Vector((0, TRI_H, 0)),
    ]
    obj = create_mesh_object("Tri_Foundation", verts, [(0, 1, 2)])
    extrude_solid(obj, THICK, "Z")
    return obj


def make_ceiling():
    s = UNIT / 2.0
    verts = [
        Vector((-s, 0, 0)),
        Vector((s, 0, 0)),
        Vector((s, UNIT, 0)),
        Vector((-s, UNIT, 0)),
    ]
    obj = create_mesh_object("Ceiling", verts, [(0, 1, 2, 3)])
    extrude_solid(obj, THICK, "Z")
    return obj


def make_tri_ceiling():
    verts = [
        Vector((-UNIT / 2, 0, 0)),
        Vector((UNIT / 2, 0, 0)),
        Vector((0, TRI_H, 0)),
    ]
    obj = create_mesh_object("Tri_Ceiling", verts, [(0, 1, 2)])
    extrude_solid(obj, THICK, "Z")
    return obj


def make_wall(full_height=True):
    h = UNIT if full_height else UNIT / 2
    s = UNIT / 2.0
    verts = [
        Vector((-s, 0, 0)),
        Vector((s, 0, 0)),
        Vector((s, 0, h)),
        Vector((-s, 0, h)),
    ]
    name = "Wall" if full_height else "Half_Wall"
    obj = create_mesh_object(name, verts, [(0, 1, 2, 3)])
    extrude_solid(obj, THICK, "Y")
    return obj


def make_doorway():
    h = UNIT
    s = UNIT / 2.0
    door_h = UNIT * 0.75
    door_w = UNIT * 0.5
    verts = [
        (-s, 0, 0), (-door_w / 2, 0, 0), (door_w / 2, 0, 0), (s, 0, 0),
        (-door_w / 2, 0, door_h), (door_w / 2, 0, door_h),
        (-s, 0, h), (s, 0, h),
    ]
    faces = [
        (0, 1, 4, 6),
        (2, 3, 7, 5),
        (4, 5, 7, 6),
    ]
    obj = create_mesh_object("Doorway", verts, faces)
    extrude_solid(obj, THICK, "Y")
    return obj


def make_window_frame():
    h = UNIT
    s = UNIT / 2.0
    win_bottom = UNIT * 0.3
    win_top = UNIT * 0.7
    win_w = UNIT * 0.6
    verts = [
        (-s, 0, 0), (s, 0, 0),
        (-win_w / 2, 0, win_bottom), (win_w / 2, 0, win_bottom),
        (-win_w / 2, 0, win_top), (win_w / 2, 0, win_top),
        (-s, 0, h), (s, 0, h),
    ]
    faces = [
        (0, 1, 3, 2),
        (2, 3, 1, 0),
        (0, 2, 4, 6),
        (3, 1, 7, 5),
        (4, 5, 7, 6),
    ]
    obj = create_mesh_object("Window_Frame", verts, faces)
    extrude_solid(obj, THICK, "Y")
    return obj


def make_wall_frame():
    h = UNIT
    s = UNIT / 2.0
    ft = 0.15  # frame thickness

    # Outer rectangle (bottom left, bottom right, top right, top left)
    v0 = Vector((-s, 0, 0))      # bottom left outer
    v1 = Vector(( s, 0, 0))      # bottom right outer
    v2 = Vector(( s, 0, h))      # top right outer
    v3 = Vector((-s, 0, h))      # top left outer

    # Inner rectangle (bottom left, bottom right, top right, top left)
    v4 = Vector((-s + ft, 0, 0))     # bottom left inner
    v5 = Vector(( s - ft, 0, 0))     # bottom right inner
    v6 = Vector(( s - ft, 0, h - ft))# top right inner
    v7 = Vector((-s + ft, 0, h - ft))# top left inner

    verts = [v0, v1, v2, v3, v4, v5, v6, v7]

    # Faces:
    # Left post:  outer bottom left → inner bottom left → inner top left → outer top left
    # Right post: outer bottom right → inner bottom right → inner top right → outer top right
    # Top beam:   outer top left → outer top right → inner top right → inner top left

    faces = [
        (0, 4, 7, 3),  # left post
        (1, 5, 6, 2),  # right post
        (3, 2, 6, 7),  # top beam
    ]

    obj = create_mesh_object("Wall_Frame", verts, faces)
    extrude_solid(obj, THICK, "Y")
    return obj





def make_roof():
    s = UNIT / 2.0
    verts = [
        Vector((-s, 0, 0)),
        Vector((s, 0, 0)),
        Vector((s, UNIT, UNIT)),
        Vector((-s, UNIT, UNIT)),
    ]
    obj = create_mesh_object("Roof", verts, [(0, 1, 2, 3)])
    extrude_solid(obj, THICK, "Y")
    return obj


def make_tri_roof():
    verts = [
        Vector((-UNIT / 2, 0, 0)),
        Vector((UNIT / 2, 0, 0)),
        Vector((0, UNIT, UNIT)),
    ]
    obj = create_mesh_object("Tri_Roof", verts, [(0, 1, 2)])
    extrude_solid(obj, THICK, "Y")
    return obj


# ---------------- helpers ----------------

def get_foundation_top_z(f):
    return f.location.z + THICK


def is_foundation(obj):
    return obj and any(n in obj.name for n in FOUNDATION_NAMES)


def is_square_foundation(obj):
    return obj and "Square_Foundation" in obj.name


def is_tri_foundation(obj):
    return obj and "Tri_Foundation" in obj.name


def get_piece_type(obj):
    if not obj:
        return None
    n = obj.name
    if "Square_Foundation" in n:
        return "SQUARE_FOUNDATION"
    if "Tri_Foundation" in n:
        return "TRI_FOUNDATION"
    if "Tri_Ceiling" in n:
        return "TRI_CEILING"
    if "Ceiling" in n:
        return "CEILING"
    if "Half_Wall" in n:
        return "HALF_WALL"
    if "Window_Frame" in n:
        return "WINDOW_FRAME"
    if "Wall_Frame" in n:
        return "WALL_FRAME"
    if "Doorway" in n:
        return "DOORWAY"
    if "Wall" in n:
        return "WALL"
    if "Roof" in n:
        return "ROOF"
    if "Tri_Roof" in n:
        return "TRI_ROOF"
    return None


def snap_world_grid(pos):
    return Vector((round(pos.x / UNIT) * UNIT, round(pos.y / UNIT) * UNIT, 0))


def snap_square(hit, f):
    rel = hit - f.location
    return f.location + Vector(
        (round(rel.x / UNIT) * UNIT, round(rel.y / UNIT) * UNIT, 0)
    )


def snap_tri_grid(hit, f):
    rel = hit - f.location
    u = Vector((UNIT, 0))
    v = Vector((UNIT / 2, TRI_H))
    mat = Matrix(((u.x, v.x), (u.y, v.y)))
    det = mat[0][0] * mat[1][1] - mat[0][1] * mat[1][0]
    if abs(det) < 1e-6:
        return f.location + rel
    inv = Matrix(
        (
            (mat[1][1] / det, -mat[0][1] / det),
            (-mat[1][0] / det, mat[0][0] / det),
        )
    )
    ab = inv @ Vector((rel.x, rel.y))
    snapped = u * round(ab.x) + v * round(ab.y)
    return f.location + Vector((snapped.x, snapped.y, 0))


def get_edges_world(obj):
    mw = obj.matrix_world
    bm = bmesh.new()
    bm.from_mesh(obj.data)
    edges = [(mw @ v.co for v in e.verts) for e in bm.edges]
    bm.free()
    return edges


def closest_on_seg(p, a, b):
    ab = b - a
    t = max(0, min(1, (p - a).dot(ab) / (ab.dot(ab) + 1e-8)))
    return a + ab * t


def find_closest_edge(hit, obj):
    edges = get_edges_world(obj)
    best_d = float("inf")
    best = None, None
    for a, b in edges:
        cp = closest_on_seg(hit, a, b)
        d = (hit - cp).length
        if d < best_d:
            best_d = d
            best = (a, b), cp
    return best


def place_on_edge(hit, f, new):
    edge, point = find_closest_edge(hit, f)
    if not edge:
        return

    a, b = edge

    # Edge direction in 2D
    edge_dir = (b - a).to_2d()
    if edge_dir.length_squared == 0:
        return
    edge_dir.normalize()
    edge_angle = atan2(edge_dir.y, edge_dir.x)

    # Edge normal in 2D
    norm2d = Vector((-edge_dir.y, edge_dir.x))

    # Flip normal based on which side the cursor is on
    cen2d = Vector((f.location.x, f.location.y))
    p2d = Vector((point.x, point.y))
    to_edge = p2d - cen2d
    if to_edge.dot(norm2d) < 0:
        norm2d *= -1

    # Base rotation: align forward to normal
    ang = atan2(norm2d.y, norm2d.x) - radians(90)
    new.rotation_euler.z = ang

    # Position at edge midpoint
    mid = (a + b) / 2
    mid.z = f.location.z

    # Square foundations offset outward
    if get_piece_type(new) == "SQUARE_FOUNDATION":
        offset = norm2d * (UNIT / 2)
        mid += Vector((offset.x, offset.y, 0))

    new.location = mid

    # Triangle-to-triangle (or tri roof/ceil) → align flat edge to edge and mirror
    pt_new = get_piece_type(new)
    pt_f = get_piece_type(f)
    if pt_new in {"TRI_FOUNDATION", "TRI_CEILING", "TRI_ROOF"} and pt_f in {
        "TRI_FOUNDATION",
        "TRI_CEILING",
        "TRI_ROOF",
    }:
        # Align triangle's flat edge (local 0°) to edge direction
        new.rotation_euler.z = edge_angle
        # Mirror so apex points outward (rhombus)
        new.rotation_euler.z += radians(180)


def get_wall_top_z(obj):
    if "Half_Wall" in obj.name:
        return obj.location.z + UNIT / 2
    return obj.location.z + UNIT


def place_roof_on_wall(hit, wall_obj, roof_obj):
    roof_obj.rotation_euler = wall_obj.rotation_euler.copy()
    top_z = get_wall_top_z(wall_obj)
    roof_obj.location = Vector(
        (wall_obj.location.x, wall_obj.location.y, top_z)
    )

    mw = wall_obj.matrix_world
    wall_forward = mw.to_quaternion() @ Vector((0, 1, 0))
    wall_center = Vector((wall_obj.location.x, wall_obj.location.y, 0))
    hit2d = Vector((hit.x, hit.y))
    center2d = Vector((wall_center.x, wall_center.y))
    to_hit = (hit2d - center2d).normalized()
    forward2d = Vector((wall_forward.x, wall_forward.y)).normalized()

    if to_hit.dot(forward2d) < 0:
        roof_obj.rotation_euler.z += radians(180)


# ---------------- ray stuff ----------------

def ray_mouse(context, event):
    reg = context.region
    rv3d = context.region_data
    coord = (event.mouse_region_x, event.mouse_region_y)
    vec = view3d_utils.region_2d_to_vector_3d(reg, rv3d, coord)
    orig = view3d_utils.region_2d_to_origin_3d(reg, rv3d, coord)
    dir = vec.normalized()
    res, loc, _, _, obj, _ = context.scene.ray_cast(
        context.view_layer.depsgraph, orig, dir
    )
    return (loc, obj) if res else (None, None)


def prioritized_ray(context, event):
    hit, obj = ray_mouse(context, event)
    if obj and get_piece_type(obj) in {
        "WALL",
        "HALF_WALL",
        "DOORWAY",
        "WINDOW_FRAME",
        "WALL_FRAME",
    }:
        return hit, obj
    return hit, obj


def ground_hit(context, event, z=0):
    reg = context.region
    rv3d = context.region_data
    coord = (event.mouse_region_x, event.mouse_region_y)
    vec = view3d_utils.region_2d_to_vector_3d(reg, rv3d, coord)
    orig = view3d_utils.region_2d_to_origin_3d(reg, rv3d, coord)
    if abs(vec.z) < 1e-6:
        return None
    t = (z - orig.z) / vec.z
    if t < 0:
        return None
    return orig + vec * t


# ---------------- main operator ----------------

class REGENESIS_OT_place_piece(bpy.types.Operator):
    bl_idname = "regenesis.place_piece"
    bl_label = "Place Build Piece"
    bl_options = {"REGISTER", "UNDO", "GRAB_CURSOR", "BLOCKING"}

    piece_type: bpy.props.StringProperty()

    def create_initial_piece(self):
        creators = {
            "SQUARE_FOUNDATION": make_square_foundation,
            "TRI_FOUNDATION": make_tri_foundation,
            "WALL": lambda: make_wall(True),
            "HALF_WALL": lambda: make_wall(False),
            "CEILING": make_ceiling,
            "TRI_CEILING": make_tri_ceiling,
            "DOORWAY": make_doorway,
            "WINDOW_FRAME": make_window_frame,
            "WALL_FRAME": make_wall_frame,
            "ROOF": make_roof,
            "TRI_ROOF": make_tri_roof,
        }
        fn = creators.get(self.piece_type)
        if not fn:
            return None
        obj = fn()
        ghost_mat = ensure_preview_material()
        if obj.data.materials:
            obj.data.materials[0] = ghost_mat
        else:
            obj.data.materials.append(ghost_mat)
        obj.show_in_front = True
        return obj

    def duplicate_preview(self):
        if not self.obj:
            return None
        bpy.ops.object.select_all(action="DESELECT")
        self.obj.select_set(True)
        bpy.context.view_layer.objects.active = self.obj
        bpy.ops.object.duplicate(linked=False)
        new_obj = bpy.context.active_object
        ghost_mat = ensure_preview_material()
        if new_obj.data.materials:
            new_obj.data.materials[0] = ghost_mat
        else:
            new_obj.data.materials.append(ghost_mat)
        new_obj.show_in_front = True
        self.obj = new_obj
        return new_obj

    def invoke(self, context, event):
        self.obj = self.create_initial_piece()
        if not self.obj:
            return {"CANCELLED"}



        context.view_layer.objects.active = self.obj
        self.obj.select_set(True)
        context.window_manager.modal_handler_add(self)
        return {"RUNNING_MODAL"}

    def modal(self, context, event):
        if event.type in {"RIGHTMOUSE", "ESC"}:
            if self.obj:
                bpy.data.objects.remove(self.obj, do_unlink=True)

            return {"CANCELLED"}

        if event.type == "R" and event.value == "PRESS":
            rot = (
                radians(60)
                if self.piece_type
                in {"TRI_FOUNDATION", "TRI_CEILING", "TRI_ROOF"}
                else radians(90)
            )
            self.obj.rotation_euler.z += rot
            return {"RUNNING_MODAL"}

        if event.type == "MOUSEMOVE":
            hit, hit_obj = prioritized_ray(context, event)
            pt = get_piece_type(self.obj)
            is_wall_like = pt in {
                "WALL",
                "HALF_WALL",
                "DOORWAY",
                "WINDOW_FRAME",
                "WALL_FRAME",
            }
            is_tri = pt in {"TRI_FOUNDATION", "TRI_CEILING", "TRI_ROOF"}
            is_ceil = pt in {"CEILING", "TRI_CEILING"}
            is_square_f = pt == "SQUARE_FOUNDATION"
            is_roof = pt in {"ROOF", "TRI_ROOF"}

            if hit_obj:
                ht = get_piece_type(hit_obj)
                is_hit_wall_like = ht in {
                    "WALL",
                    "HALF_WALL",
                    "DOORWAY",
                    "WINDOW_FRAME",
                    "WALL_FRAME",
                }

                # Roofs on top of walls / half walls
                if ht in {"WALL", "HALF_WALL"} and is_roof:
                    place_roof_on_wall(hit, hit_obj, self.obj)
                    return {"RUNNING_MODAL"}

                # Ceilings on top of walls / frames / doors
                if is_hit_wall_like and is_ceil:
                    self.obj.location = Vector(
                        (
                            hit_obj.location.x,
                            hit_obj.location.y,
                            get_wall_top_z(hit_obj) - THICK,
                        )
                    )
                    self.obj.rotation_euler = hit_obj.rotation_euler.copy()
                    return {"RUNNING_MODAL"}

                # Walls/doors/frames stacked on top of each other
                if is_hit_wall_like and is_wall_like:
                    self.obj.location = Vector(
                        (
                            hit_obj.location.x,
                            hit_obj.location.y,
                            get_wall_top_z(hit_obj),
                        )
                    )
                    self.obj.rotation_euler = hit_obj.rotation_euler.copy()
                    return {"RUNNING_MODAL"}

                # Walls/doors/frames on top edges of ceilings
                if ht in {"CEILING", "TRI_CEILING"} and is_wall_like:
                    place_on_edge(hit, hit_obj, self.obj)
                    # Move wall up so it sits on top of the ceiling
                    self.obj.location.z = hit_obj.location.z + THICK
                    return {"RUNNING_MODAL"}

                # Foundations / edge snapping
                if is_foundation(hit_obj):
                    if is_square_foundation(hit_obj):
                        if is_tri and pt == "TRI_FOUNDATION":
                            place_on_edge(hit, hit_obj, self.obj)
                            return {"RUNNING_MODAL"}
                        if is_ceil and pt == "CEILING":
                            self.obj.location = snap_square(hit, hit_obj)
                            return {"RUNNING_MODAL"}
                        if is_wall_like or is_roof:
                            place_on_edge(hit, hit_obj, self.obj)
                            self.obj.rotation_euler.z += radians(180)
                            self.obj.location.z = get_foundation_top_z(hit_obj)
                            return {"RUNNING_MODAL"}
                        self.obj.location = snap_square(hit, hit_obj)

                    elif is_tri_foundation(hit_obj):
                        if is_square_f:
                            place_on_edge(hit, hit_obj, self.obj)
                            return {"RUNNING_MODAL"}
                        if is_ceil and pt == "TRI_CEILING":
                            self.obj.location = snap_tri_grid(hit, hit_obj)
                            return {"RUNNING_MODAL"}
                        if is_wall_like or is_roof:
                            place_on_edge(hit, hit_obj, self.obj)
                            self.obj.rotation_euler.z += radians(180)
                            self.obj.location.z = get_foundation_top_z(hit_obj)
                            return {"RUNNING_MODAL"}
                        if pt == "TRI_FOUNDATION":
                            place_on_edge(hit, hit_obj, self.obj)
                            return {"RUNNING_MODAL"}
                        self.obj.location = snap_tri_grid(hit, hit_obj)

            else:
                if pt in {"SQUARE_FOUNDATION", "TRI_FOUNDATION"}:
                    g = ground_hit(context, event)
                    if g:
                        self.obj.location = snap_world_grid(g)
                        self.obj.rotation_euler = (0, 0, 0)

        if event.type == "LEFTMOUSE" and event.value == "PRESS":
            solid_mat = ensure_solid_material()
            if self.obj.data.materials:
                self.obj.data.materials[0] = solid_mat
            else:
                self.obj.data.materials.append(solid_mat)
            self.obj.show_in_front = False

            if self.duplicate_preview():
                return {"RUNNING_MODAL"}
            else:

                return {"FINISHED"}

        return {"RUNNING_MODAL"}


# ─── PIE MENU PAGING SYSTEM ────────────────────────────────────────────────

current_pie_page = 0

PIE_PAGES = [
    # PAGE 0 — FOUNDATIONS + WALLS
    [
        ("Square", "SQUARE_FOUNDATION"),
        ("Tri", "TRI_FOUNDATION"),
        ("Wall", "WALL"),
        ("Half", "HALF_WALL"),
        ("Door", "DOORWAY"),
        ("Window", "WINDOW_FRAME"),
    ],

    # PAGE 1 — CEILINGS + ROOFS + FRAMES
    [
        ("Ceiling", "CEILING"),
        ("TriCeil", "TRI_CEILING"),
        ("Roof", "ROOF"),
        ("TriRoof", "TRI_ROOF"),
        ("Frame", "WALL_FRAME"),
    ],
]



def clamp_page():
    global current_pie_page
    if current_pie_page < 0:
        current_pie_page = len(PIE_PAGES) - 1
    elif current_pie_page >= len(PIE_PAGES):
        current_pie_page = 0


class REGENESIS_OT_pie_next(bpy.types.Operator):
    bl_idname = "regenesis.pie_next_page"
    bl_label = "Next Pie Page"

    def execute(self, context):
        global current_pie_page
        current_pie_page += 1
        clamp_page()
        bpy.ops.wm.call_menu_pie(name="REGENESIS_MT_build_pie")
        return {"FINISHED"}


class REGENESIS_OT_pie_prev(bpy.types.Operator):
    bl_idname = "regenesis.pie_prev_page"
    bl_label = "Previous Pie Page"

    def execute(self, context):
        global current_pie_page
        current_pie_page -= 1
        clamp_page()
        bpy.ops.wm.call_menu_pie(name="REGENESIS_MT_build_pie")
        return {"FINISHED"}


# ---------------- pie ----------------

class REGENESIS_MT_build_pie(bpy.types.Menu):
    bl_label = "Regenesis Shit"
    bl_idname = "REGENESIS_MT_build_pie"

    def draw(self, context):
        global current_pie_page
        pie = self.layout.menu_pie()

        page = PIE_PAGES[current_pie_page]

        for label, piece in page:
            pie.operator("regenesis.place_piece", text=label).piece_type = piece

        pie.operator("regenesis.pie_prev_page", text="<< Prev")
        pie.operator("regenesis.pie_next_page", text="Next >>")


class REGENESIS_OT_call_build_pie(bpy.types.Operator):
    bl_idname = "regenesis.call_build_pie"
    bl_label = "Call Build Pie"

    def execute(self, context):
        bpy.ops.wm.call_menu_pie(name="REGENESIS_MT_build_pie")
        return {"FINISHED"}


classes = (
    REGENESIS_OT_place_piece,
    REGENESIS_OT_pie_next,
    REGENESIS_OT_pie_prev,
    REGENESIS_MT_build_pie,
    REGENESIS_OT_call_build_pie,
)

addon_keymaps = []


def register():
    for c in classes:
        bpy.utils.register_class(c)
    wm = bpy.context.window_manager
    kc = wm.keyconfigs.addon
    if kc:
        km = kc.keymaps.new(name="3D View", space_type="VIEW_3D")
        kmi = km.keymap_items.new("regenesis.call_build_pie", "Q", "PRESS")
        addon_keymaps.append((km, kmi))


def unregister():
    for km, kmi in addon_keymaps:
        km.keymap_items.remove(kmi)
    addon_keymaps.clear()
    for c in reversed(classes):
        bpy.utils.unregister_class(c)


if __name__ == "__main__":
    register()
